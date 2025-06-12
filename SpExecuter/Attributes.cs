using Microsoft.Extensions.DependencyInjection;
using System;

namespace SpExecuter
{
    //On Interface For registring for Implementation
    [AttributeUsage(AttributeTargets.Interface, AllowMultiple = false)]
    public sealed class SpHandler : Attribute
    {
        private readonly ServiceLifetime LifeTime;
        public SpHandler(ServiceLifetime serviceLifetime)
        {
            LifeTime = serviceLifetime;
        }
    }
    //On Method for Actual DB call implementation
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = false)]
    public sealed class StoredProcedure : Attribute
    {
        private readonly string StoredProcedureName;
        public StoredProcedure(string storedProcedureName)
        {
            StoredProcedureName = storedProcedureName;
        }
    }
    //On Class for declaring as Table Valued Parameter (TVP)
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public sealed class TVP : Attribute
    {
        private string TVPName;
        public TVP(string tVPName)
        {
            TVPName = tVPName;
        }
    }
    //On string[] property for TVP table generation
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
    public sealed class TVPType : Attribute
    {
        private Type TVPClassType;
        public TVPType(Type tVPClassType)
        {
            TVPClassType = tVPClassType;
        }
    }

}