using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Microsoft.Extensions.DependencyInjection;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace SpExecuter
{

    [Generator]
    public class SpHandlerGenerator : IIncrementalGenerator
    {
        public void Initialize(IncrementalGeneratorInitializationContext context)
        {
            // This adds a dummy trigger that always fires once per compilation
            var provider = context.AnalyzerConfigOptionsProvider;

            // 1. Filter syntax to get class declarations with attributes
            var classDeclarations = context.SyntaxProvider
                .CreateSyntaxProvider(
                    predicate: (s, _) => IsCandidateInterface(s),
                    transform: (ctx, _) => GetClassInfo(ctx));

            // 2. Collect into a single input
            var compilationAndClasses = context.CompilationProvider.Combine(classDeclarations.Collect());
            //For Generating Implementation classes
            Dictionary<string, StringBuilder> allClassSyntax = new Dictionary<string, StringBuilder>();

            //For generating extension method for registring interfaces and classes for DI
            Dictionary<string, (string, ServiceLifetime)> registerClasses = new Dictionary<string, (string, ServiceLifetime)>();
            StringBuilder buildServices = new StringBuilder();

            //For Generating Response class conatining SP Response class info
            StringBuilder responseClassConfiguration = new StringBuilder();
            HashSet<string> uniqueResponseClasses = new HashSet<string>();

            //For Generating Response class conatining SP Request class info
            StringBuilder requestClassConfiguration = new StringBuilder();
            HashSet<string> uniqueRequestClasses = new HashSet<string>();
            // 3. Use the result to generate code
            context.RegisterSourceOutput(compilationAndClasses, (ctx, pair) =>
            {
                var (compilation, interfaces) = pair;
                GeneratedExecuterClasses(allClassSyntax,
                    compilation, interfaces, registerClasses, uniqueResponseClasses, uniqueRequestClasses);
                /* if (registerClasses.Count() < 1)
                 {
                     return;
                 }*/
                GenerateRequestClassesConfiguration(uniqueRequestClasses, requestClassConfiguration);
                ctx.AddSource($"{"RequestClasses"}Generated.g.cs", SourceText.From(requestClassConfiguration.ToString(), Encoding.UTF8));

                GenerateResponseClassesConfiguration(uniqueResponseClasses, responseClassConfiguration);
                ctx.AddSource($"{"ResponseClasses"}Generated.g.cs", SourceText.From(responseClassConfiguration.ToString(), Encoding.UTF8));

                foreach (KeyValuePair<string, StringBuilder> generateClass in allClassSyntax)
                {
                    ctx.AddSource($"{generateClass.Key}Generated.g.cs", SourceText.From(generateClass.Value.ToString(), Encoding.UTF8));
                }
            });

        }


        private static void GeneratedExecuterClasses(Dictionary<string, StringBuilder> allClassSyntax,
            Compilation compilation, ImmutableArray<InterfaceInfo> interfaces,
            Dictionary<string, (string, ServiceLifetime)> registerClasses, HashSet<string> uniqueReturnClasses,
              HashSet<string> uniqueRequestClasses)
        {


            INamedTypeSymbol spHandlerAttrSymbol = compilation.GetTypeByMetadataName("SpExecuter.SPHandler");
            if (spHandlerAttrSymbol is null)
            { return; }

            foreach (InterfaceInfo classInfo in interfaces)
            {
                INamedTypeSymbol interfaceSymbol = classInfo.Symbol;
                foreach (AttributeData attr in interfaceSymbol.GetAttributes())
                {
                    //If Interface found then only generate classes
                    if (SymbolEqualityComparer.Default.Equals(attr.AttributeClass, spHandlerAttrSymbol))
                    {

                        // Get interface name and class name for DI
                        string className = GetClassToRegister(registerClasses, interfaceSymbol.Name, attr);
                        string namespaceName = interfaceSymbol.ContainingNamespace?.ToDisplayString();

                        //Get class declaration syntax
                        StringBuilder classSyntax = new StringBuilder();
                        GetInitialSyntax(classSyntax,
                            namespaceName ?? "SpExecuter", className, interfaceSymbol.Name);

                        //Loop and Add Methods in class declared in interface
                        foreach (IMethodSymbol member in interfaceSymbol.GetMembers().OfType<IMethodSymbol>())
                        {
                            //Get Method Metadata for generating Declaration
                            string returnType = member.ReturnType.ToDisplayString();
                            string methodName = member.Name;

                            string parameters = string.Join(", ",
                                member.Parameters.Select(p => $"{p.Type.ToDisplayString()} {p.Name}"));
                            //Get StoredProcedure Name
                            string spName = string.Empty;
                            foreach (AttributeData methodAttribute in member.GetAttributes())
                            {
                                INamedTypeSymbol methodAttributeSymbol = methodAttribute.AttributeClass;
                                if (methodAttributeSymbol?.Name == "StoredProcedure")
                                {
                                    TypedConstant spNameArg = methodAttribute.ConstructorArguments[0];
                                    spName = spNameArg.Value as string ?? "";
                                }
                            }

                            string connctionStringParamName = member.Parameters[0].Name;
                            string objectParamName = "null";
                            string requestTypeName = "NoRequest";
                            string paramNeeded = "false";

                            if (member.Parameters.Length > 1)
                            {
                                objectParamName = member.Parameters[1].Name;
                                requestTypeName = member.Parameters[1].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                                uniqueRequestClasses.Add(requestTypeName);

                                paramNeeded = "true";
                            }
                            string returnTypes = GetReturnTypeStringForExecuter(member, uniqueReturnClasses);
                            string returnStatement = GenerateReturnStatement(member.ReturnType, compilation);

                            // Generate Method Implementation
                            classSyntax.AppendLine($"    public async {returnType} {methodName}({parameters})");
                            classSyntax.AppendLine("    {");
                            classSyntax.AppendLine("        List<ISpResponse>[] response =await SpExecutor.ExecuteSpToObjects(");
                            classSyntax.AppendLine($"           spName: {spName}, dbName: {connctionStringParamName}, spNeedParameters: {paramNeeded},spEntity: {objectParamName}, ");
                            classSyntax.AppendLine($"           requestObjectNumber: SpRequest.{requestTypeName} ");
                            classSyntax.AppendLine($"           {returnTypes} );");
                            classSyntax.AppendLine($"         return {returnStatement}");
                            classSyntax.AppendLine("    }");
                            classSyntax.AppendLine();
                        }

                        //Class implementation complete
                        classSyntax.AppendLine("}");
                        allClassSyntax.Add(className, classSyntax);
                    }
                }
            }
        }
        private static string GenerateReturnStatement(ITypeSymbol returnType, Compilation compilation)
        {
            // 1) Unwrap Task<T> or ValueTask<T>
            if (returnType is INamedTypeSymbol named)
            {
                var taskSym = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1");
                var valueTaskSym = compilation.GetTypeByMetadataName("System.Threading.Tasks.ValueTask`1");
                if (SymbolEqualityComparer.Default.Equals(named.OriginalDefinition, taskSym))
                {
                    returnType = named.TypeArguments[0];
                }
                else if (SymbolEqualityComparer.Default.Equals(named.OriginalDefinition, valueTaskSym))
                {
                    returnType = named.TypeArguments[0];
                }
            }

            string returnExpr;
            // 2) Check for tuple return
            if (returnType.IsTupleType && returnType is INamedTypeSymbol namedTuple)
            {
                var underlying = namedTuple.TupleUnderlyingType;
                var types = underlying.TypeArguments;
                var tupleParts = new List<string>();
                for (int i = 0; i < types.Length; i++)
                {
                    var elemType = types[i];
                    // If tuple element is List<T>
                    if (elemType is INamedTypeSymbol namedElem
                        && SymbolEqualityComparer.Default.Equals(namedElem.OriginalDefinition,
                                                                compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")))
                    {
                        tupleParts.Add($"(List<{namedElem.TypeArguments[0].ToDisplayString()}>)(response[{i}])");
                    }
                    else
                    {
                        tupleParts.Add($"({elemType.ToDisplayString()})response[{i}][0]");
                    }
                }
                returnExpr = "(" + string.Join(", ", tupleParts) + ")";
            }
            // 3) Check for List<T> return
            else if (returnType is INamedTypeSymbol namedList
                     && SymbolEqualityComparer.Default.Equals(namedList.OriginalDefinition,
                                                             compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")))
            {
                var elemType = namedList.TypeArguments[0];
                returnExpr = $"(List<{elemType.ToDisplayString()}>)(response[0])";
            }
            // 4) Single object or primitive
            else
            {
                returnExpr = $"({returnType.ToDisplayString()})response[0][0]";
            }

            return "return " + returnExpr + ";";
        }

        private static void GenerateRequestClassesConfiguration(HashSet<string> uniqueRequestClasses, StringBuilder builder)
        {
            builder.AppendLine($"namespace SpExecuter;"); builder.AppendLine();
            builder.AppendLine($"public class SpRequest");
            builder.AppendLine("{");
            builder.AppendLine();
            builder.AppendLine($"public const int NoRequest = 0 ;");
            builder.AppendLine();
            int srNo = 1;
            foreach (string requestClass in uniqueRequestClasses)
            {
                builder.AppendLine($"public const int {requestClass} = {srNo};");
                builder.AppendLine();
                srNo++;
            }
            builder.AppendLine("}");
        }
        private static void GenerateResponseClassesConfiguration(HashSet<string> uniqueResponseCasses, StringBuilder builder)
        {
            builder.AppendLine($"namespace SpExecuter;"); builder.AppendLine();
            builder.AppendLine($"public class SpResponse");
            builder.AppendLine("{");
            builder.AppendLine();
            builder.AppendLine($"public const int SkipResponse = 0 ;");
            builder.AppendLine();

            int srNo = 1;
            foreach (string responseClass in uniqueResponseCasses)
            {
                builder.AppendLine($"public const int {responseClass} = {srNo} ;");
                builder.AppendLine();
                srNo++;
            }
            builder.AppendLine("}");
        }
        private static void CollectUserDefinedClasses(
                ITypeSymbol typeSymbol,
                List<string> outSet)
        {

            if (typeSymbol == null)
                return;

            // 1) If this is a named type (which includes tuples, generics, and non‐generic classes)
            if (typeSymbol is INamedTypeSymbol namedSym)
            {
                // 1a) TUPLE‐case: IsTupleType == true means this is e.g. (A, B) or (A, B, C, …)
                if (namedSym.IsTupleType)
                {
                    foreach (var element in namedSym.TupleElements)
                    {
                        CollectUserDefinedClasses(element.Type, outSet);
                    }
                    return;
                }

                // 1b) GENERIC‐case: unwrap all type arguments (e.g. ValueTask<T>, List<T>, Dictionary<A,B>, etc.)
                if (namedSym.IsGenericType)
                {
                    foreach (var typeArg in namedSym.TypeArguments)
                    {
                        CollectUserDefinedClasses(typeArg, outSet);
                    }
                    return;
                }

                // 1c) PLAIN CLASS (non‐generic, non‐tuple):
                //     If it’s a “user‐declared” class (i.e. TypeKind.Class + has DeclaringSyntaxReferences),
                //     add it to the set.
                if (namedSym.TypeKind == TypeKind.Class &&
                    namedSym.DeclaringSyntaxReferences.Length > 0)
                {
                    outSet.Add(namedSym.Name);
                }

                // 1d) ARRAY disguised as namedSym (rare):
                //     Sometimes arrays show up as IArrayTypeSymbol, but we handle that below.
                if (namedSym.TypeKind == TypeKind.Array &&
                    namedSym is IArrayTypeSymbol arraySym)
                {
                    CollectUserDefinedClasses(arraySym.ElementType, outSet);
                }

                return;
            }

            // 2) If it’s an array type (caught here if not already handled above)
            if (typeSymbol is IArrayTypeSymbol arrayType)
            {
                CollectUserDefinedClasses(arrayType.ElementType, outSet);
                return;
            }

            // 3) Otherwise (primitive, pointer, type parameter, etc.) → ignore
        }
        private static string GetReturnTypeStringForExecuter(IMethodSymbol member, HashSet<string> uniqueReturnClasses)
        {
            //GETMethods return type classes and add them in Hashset
            List<string> currentMethodsReturnTypes = new List<string>();
            CollectUserDefinedClasses(member.ReturnType, currentMethodsReturnTypes);
            // Transform each item to "SpResponse.ClassX"
            string[] responseArray = currentMethodsReturnTypes
                .Select(item => $"SpResponse.{item}")
                .ToArray();
            string returnTypes = string.Join(", ", responseArray);
            returnTypes = string.IsNullOrEmpty(returnTypes) ? ", " + returnTypes : string.Empty;
            foreach (string eachReturnType in currentMethodsReturnTypes)
            {
                uniqueReturnClasses.Add(eachReturnType);
            }
            return returnTypes;
        }



        private static void GetInitialSyntax(StringBuilder builder,
            string namespaceName, string className, string interfaceName)
        {
            builder.AppendLine("using System.Text;");
            builder.AppendLine("using SpExecuter;");
            builder.AppendLine($"namespace {namespaceName};");
            builder.AppendLine($"public class {className} : {interfaceName}");
            builder.AppendLine("{");

        }
        private static string GetClassToRegister(Dictionary<string, (string, ServiceLifetime)> pairs,
            string interfaceName, AttributeData attr)
        {
            string className = string.Empty;
            if (interfaceName[0] == 'I' || interfaceName[0] == 'i')
            {
                className = interfaceName.Substring(1);

            }
            else
            {
                className = interfaceName+"Class";

            }
            TypedConstant lifeTime = attr.ConstructorArguments[0];
            var lifetime = lifeTime.Value is int intValue
                            ? (ServiceLifetime)intValue
                            : ServiceLifetime.Singleton;
            pairs.Add(interfaceName, (className, lifetime));
            return className;
        }
        private static bool IsCandidateInterface(SyntaxNode node)
        {
            return node is InterfaceDeclarationSyntax interfaceDecl && interfaceDecl.AttributeLists.Count > 0;
        }

        private static InterfaceInfo GetClassInfo(GeneratorSyntaxContext context)
        {
            var interfaceDecl = (InterfaceDeclarationSyntax)context.Node;
            var symbol = context.SemanticModel.GetDeclaredSymbol(interfaceDecl) as INamedTypeSymbol;

            if (symbol == null)
                return null;

            return new InterfaceInfo
            {
                InterfaceDeclaration = interfaceDecl,
                Symbol = symbol
            };
        }

        private class InterfaceInfo
        {
            public InterfaceDeclarationSyntax InterfaceDeclaration { get; set; }
            public INamedTypeSymbol Symbol { get; set; }
        }

    }
}









