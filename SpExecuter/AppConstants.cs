using Sigil;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;



namespace SpExecuter
{
    

    internal class AppConstants
    {
        #region Constants
        public const string AtTheRate = "@";
        public const string UnderScore = "_";
        #endregion

        public static PropertyInfo[][] SpRequestPropertyInfoCache { get; set; } 
        public static PropertyInfo[][] SpResponsePropertyInfoCache { get; set; } 
        public static Type[] SpRequestModelTypeArray { get; set; } = new Type[0];
        public static Type[] SpResponseModelTypeArray { get; set; } = new Type[0];
        public static Type ListType { get; } = typeof(List<>);
        public static Dictionary<string, Func<object, object>>[] CachedPropertyAccessorDelegates { get; set; } 
        public static Dictionary<string, Action<object, object>>[] CachedPropertySetterDelegates { get; set; } 

        #region Getter & Setter

        public static Func<object, object> CreateGetter(Type targetType,
            string propertyName, PropertyInfo propertyInfo)
        {
            // Emit IL for a method that gets the value of the property
            var emitter = Emit<Func<object, object>>.NewDynamicMethod($"{targetType.Name}{UnderScore}{propertyName}")
                .LoadArgument(0)
                // Cast it from object to the specific type
                .CastClass(targetType)
                // Call the property getter
                .Call(propertyInfo.GetGetMethod(true));
            if (propertyInfo.PropertyType.IsValueType)
            {
                emitter.Box(propertyInfo.PropertyType);
            }
            // Return the property value
            emitter.Return();
            // Compile the IL code into a delegate and return it
            return emitter.CreateDelegate();

        }
        public static Action<object, object> CreateSetter(Type targetType,
        string propertyName, PropertyInfo propertyInfo)
        {
            // Prepare a DynamicMethod-backed emitter for Action<object,object>
            var emitter = Emit<Action<object, object>>.NewDynamicMethod(
                    $"{targetType.Name}{UnderScore}{propertyName}")
                // 1st arg: the target instance (as object)
                .LoadArgument(0)
                // cast it to the actual declaring type
                .CastClass(targetType)
                // 2nd arg: the value to set (as object)
                .LoadArgument(1);

            // if the property is a value-type, unbox; otherwise cast
            if (propertyInfo.PropertyType.IsValueType)
                emitter.UnboxAny(propertyInfo.PropertyType);
            else
                emitter.CastClass(propertyInfo.PropertyType);

            // call the property's set method
            emitter.Call(propertyInfo.GetSetMethod(true));

            // return (void)
            emitter.Return();

            // compile and return the delegate
            return emitter.CreateDelegate();
        }
        #endregion
    }
    public interface ISpResponse { }
    public class SkipResponse { }
    public class GenericSpResponse : ISpResponse
    {
        public int NumberOfRowsAffected { get; set; } = 0;
        public bool IsSuccess { get; set; } = true;
    }
    public class SpExecuterException : Exception
    {
        public StringBuilder Information { get; set; }

        public SpExecuterException(StringBuilder info, Exception ex) : base(ex.Message, ex)
        {
            Information = info;
        }
    }
}
