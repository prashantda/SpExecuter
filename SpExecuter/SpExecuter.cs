using Microsoft.Data.SqlClient;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Utf8Json;



namespace SpExecuter
{
   
    internal class SpExecutor
    {
        public async static Task<List<ISpResponse>[]> ExecuteSpToObjects(string spName,
            string dbName = default, bool spNeedParameters = true, object spEntity = default,
            List<SqlParameter> param = default, int requestObjectNumber = 0, params int[] returnObjects)
        {

            if (spNeedParameters && param == null && spEntity != null)
            {
                param = GetParamFromObject(spEntity, requestObjectNumber);
            }
            List<ISpResponse>[] allTables = new List<ISpResponse>[returnObjects == null ? 1 : returnObjects.Length + 1];
            GenericSpResponse genericDbResponse = new GenericSpResponse();
            try
            {
                using (SqlConnection connection = new SqlConnection(dbName))
                {
                    connection.Open();
                    using (SqlCommand command = new SqlCommand(spName, connection))
                    {
                        command.CommandType = CommandType.StoredProcedure;
                        command.Parameters.AddRange(param != null ? param.ToArray() : Array.Empty<SqlParameter>());
                        if (returnObjects == null)
                        {
                            genericDbResponse.NumberOfRowsAffected = await command.ExecuteNonQueryAsync();
                        }
                        else
                        {
                            using (SqlDataReader reader = await command.ExecuteReaderAsync())
                            {
                                for (int i = 0; i < returnObjects.Length; i++)
                                {

                                    int objectTypeIndex = (int)returnObjects[i];
                                    if (objectTypeIndex == 0)
                                    {
                                        reader.NextResult();
                                        continue;
                                    }
                                    Type type = AppConstants.SpResponseModelTypeArray[objectTypeIndex];
                                    var list = new List<ISpResponse>();
                                    while (reader.Read())
                                    {
                                        ISpResponse dbResponseObject = (ISpResponse)Activator.CreateInstance(type);
                                        for (int j = 0; j < AppConstants.SpResponsePropertyInfoCache[objectTypeIndex].Length; j++)
                                        {
                                            PropertyInfo prop = AppConstants.SpResponsePropertyInfoCache[objectTypeIndex][j];
                                            bool isNotNull = !reader.IsDBNull(j);
                                            if (isNotNull)
                                            {
                                                if (prop.PropertyType == typeof(string))
                                                {

                                                    prop.SetValue(dbResponseObject, reader.GetString(j));
                                                }
                                                else if (prop.PropertyType == typeof(int))
                                                {
                                                    prop.SetValue(dbResponseObject, reader.GetInt32(j));
                                                }
                                                else if (prop.PropertyType == typeof(bool))
                                                {
                                                    prop.SetValue(dbResponseObject, Convert.ToBoolean(reader.GetValue(j)));
                                                }
                                                else if (prop.PropertyType == typeof(long))
                                                {
                                                    prop.SetValue(dbResponseObject, reader.GetInt64(j));
                                                }
                                                else if (prop.PropertyType == typeof(DateTime))
                                                {
                                                    prop.SetValue(dbResponseObject, reader.GetDateTime(j));

                                                }


                                            }
                                        }
                                        list.Add(dbResponseObject);
                                    }
                                    allTables[i + 1] = list;
                                    reader.NextResult();
                                }

                            }
                        }
                        allTables[0] = new List<ISpResponse>() { genericDbResponse };

                    }
                }

            }
            catch (Exception ex)
            {
                StringBuilder info = new StringBuilder().Append(spName).Append("\r\n").Append(dbName).
                    Append("\r\n").Append(JsonSerializer.ToJsonString(returnObjects)).Append("\r\n")
                    .Append(JsonSerializer.ToJsonString(spEntity)).Append("\r\n");
                foreach (var oneparam in param)
                {
                    info.Append(oneparam.ParameterName).Append(oneparam.Value);
                }

                throw new SpExecuterException(info, ex);

            }
            return allTables;
        }
        public static List<SqlParameter> GetParamFromObject(object obj, int requestObjectNumber)
        {
            List<SqlParameter> paramList = new List<SqlParameter>();
            PropertyInfo[] spParameters = AppConstants.SpRequestPropertyInfoCache[requestObjectNumber];
            SqlParameter param;
            foreach (var parameter in spParameters)
            {
                string value = Convert.ToString(AppConstants.CachedPropertyAccessorDelegates[requestObjectNumber][parameter.Name](obj));
                object val;
                if(string.IsNullOrWhiteSpace(value))
                {
                    val = value;
                }
                else
                {
                    val = DBNull.Value;
                }
                    param = new SqlParameter($"{AppConstants.AtTheRate}{parameter.Name}",val );
                paramList.Add(param);
            }
            return paramList;
        }
    }

}
