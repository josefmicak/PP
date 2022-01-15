using System;

namespace ImmutableArray
{
    class Program
    {
        static void Main(string[] args)
        {
            //addressMain array; size = 27
            object[] addressMain = new object[3];
            object[] addressLeft = new object[3];
            object[] addressMiddle = new object[3];
            object[] addressRight = new object[3];

            addressMain[0] = addressLeft;
            addressMain[1] = addressMiddle;
            addressMain[2] = addressRight;

            addressLeft[0] = new char[3] { 'a', 'b', 'c' };
            addressLeft[1] = new char[3] { 'd', 'e', 'f' };
            addressLeft[2] = new char[3] { 'g', 'h', 'i' };

            addressMiddle[0] = new char[3] { 'j', 'k', 'l' };
            addressMiddle[1] = new char[3] { 'm', 'n', 'o' };
            addressMiddle[2] = new char[3] { 'p', 'q', 'r' };

            addressRight[0] = new char[3] { 's', 't', 'u' };
            addressRight[1] = new char[3] { 'v', 'w', 'x' };
            addressRight[2] = new char[3] { 'y', 'z', '!' };

            //addressMainNew array; size = 81
            object[] addressMainNew = new object[3];
            object[] addressLeftNew = new object[3];
            object[] addressMiddleNew = new object[3];
            object[] addressRightNew = new object[3];

            addressMainNew[0] = addressLeftNew;
            addressMainNew[1] = addressMiddleNew;
            addressMainNew[2] = addressRightNew;

            char[] testObject = new char[3] { '1', '2', '3' };

            addressLeftNew[0] = new object[3] { testObject, testObject, testObject };
            addressLeftNew[1] = new object[3] { testObject, testObject, testObject };
            addressLeftNew[2] = new object[3] { testObject, testObject, testObject };

            addressMiddleNew[0] = new object[3] { testObject, testObject, testObject };
            addressMiddleNew[1] = new object[3] { testObject, testObject, testObject };
            addressMiddleNew[2] = new object[3] { testObject, testObject, testObject };

            addressRightNew[0] = new object[3] { testObject, testObject, testObject };
            addressRightNew[1] = new object[3] { testObject, testObject, testObject };
            addressRightNew[2] = new object[3] { testObject, testObject, testObject };

            Console.WriteLine("Josef Micak (MIC0378) - Homework 4 (Immutable array), Programming paradigms, Winter semester 2021/2022.");

            int key = 14;
            char value = 'a';
            int size = 27;
            object[] arrayToUse = addressMain;

            Console.WriteLine("Element on {0}. index: {1}", key, (char)ReadValue(arrayToUse, key, size));

            arrayToUse = (object[])WriteValue(arrayToUse, key, value, size);
            Console.WriteLine("Changed element on {0}. index to {1}", key, value);

            Console.WriteLine("Element on {0}. index: {1}", key, (char)ReadValue(arrayToUse, key, size));
        }

        static object ReadValue(object[] addressMain, double key, double size)
        {
            if ((key / size) < (1.0/3.0))
            {
                try
                {
                    return ReadValue((object[])addressMain[0], key, size / 3);
                }
                catch
                {
                    return GetData((char[])addressMain[0], key);
                }
            }
            else if ((key / size) < (2.0/3.0))
            {
                try
                {
                    return ReadValue((object[])addressMain[1], key - (size / 3), size / 3);
                }
                catch
                {
                    return GetData((char[])addressMain[1], key);
                }
            }
            else if ((key / size) < 1.0)
            {
                try
                {
                    return ReadValue((object[])addressMain[2], key - (size / 3) * 2, size / 3);
                }
                catch
                {
                    return GetData((char[])addressMain[2], key);
                }
            }
            else
            {
                return ReadValue(addressMain, key % size, size);
            }
        }

        static char GetData(char[] dataTriplet, double key)
        {
            return dataTriplet[Convert.ToInt32(key) % 3];
        }

        static object WriteValue(object[] addressMain, double key, char value, double size)
        {
            if ((key / size) < (1.0 / 3.0))
            {
                try
                {
                    return new object[3] { WriteValue((object[])addressMain[0], key, value, size / 3), addressMain[1], addressMain[2]};
                }
                catch
                {
                    return SetData((char[])addressMain[0], key, value);
                }
            }
            else if ((key / size) < (2.0 / 3.0))
            {
                try
                {
                    return new object[3] { addressMain[0], WriteValue((object[])addressMain[1], key - (size / 3), value, size / 3), addressMain[2] };
                }
                catch
                {
                    return SetData((char[])addressMain[1], key, value);
                }
            }
            else if ((key / size) < 1.0)
            {
                try
                {
                    return new object[3] { addressMain[0], addressMain[1], WriteValue((object[])addressMain[2], key - (size / 3), value, size / 3) };
                }
                catch
                {
                    return SetData((char[])addressMain[2], key, value);
                }
            }
            else
            {
                return WriteValue(addressMain, key % size, value, size);
            }
        }

        static char[] SetData(char[] dataTriplet, double key, char value)
        {
            if(key % 3 == 0)
            {
                return new char[3] { value, dataTriplet[1], dataTriplet[2] };
            }
            else if (key % 3 == 1)
            {
                return new char[3] { dataTriplet[0], value, dataTriplet[2] };
            }
            else
            {
                return new char[3] { dataTriplet[0], dataTriplet[1], value };
            }
        }
    }
}
