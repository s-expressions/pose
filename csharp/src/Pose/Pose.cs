// Copyright 2021 Lassi Kortela
// Copyright 2021 Oskar Gewalli
// SPDX-License-Identifier: ISC
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Text;
using System.Threading.Tasks;

namespace Pose
{
    public interface IExpression
    {
        Task Write(StreamWriter wr);
    }
    public readonly struct List : IExpression
    {
        public List(IReadOnlyList<IExpression> value)
        {
            Value = value;
        }

        public IReadOnlyList<IExpression> Value { get; }


        public async Task Write(StreamWriter wr)
        {
            var n = Value.Count;
            if (n == 0)
            {
                await wr.WriteAsync("()");
                return;
            }

            var prefix = "(";
            for (int i = 0; i < n; i++)
            {
                await wr.WriteAsync(prefix);
                await Value[i].Write(wr);
                prefix = " ";
            }
            await wr.WriteAsync(")");
        }
    }
    public readonly struct Symbol : IExpression
    {
        public Symbol(string value)
        {
            Value = value;
        }

        public string Value { get; }
        public async Task Write(StreamWriter wr)
        {
            await wr.WriteAsync(Value);
        }
    }
    public readonly struct String : IExpression
    {
        public String(string value)
        {
            Value = value;
        }

        public string Value { get; }
        public async Task Write(StreamWriter wr)
        {
            await wr.WriteAsync("\"" + Value + "\"");
        }
    }
    public readonly struct Float64 : IExpression
    {
        public Float64(double value)
        {
            Value = value;
        }

        public double Value { get; }
        public async Task Write(StreamWriter wr)
        {
            // TODO: what should the output be here?
            await wr.WriteAsync($"{Value}");
        }
    }
    public readonly struct FixInt : IExpression
    {
        public FixInt(int value)
        {
            Value = value;
        }

        public int Value { get; }
        public async Task Write(StreamWriter wr)
        {
            // TODO: what should the output be here?
            await wr.WriteAsync($"{Value}");
        }
    }
    public readonly struct BigInt : IExpression
    {
        public BigInt(long value)
        {
            Value = value;
        }

        public long Value { get; }
        public async Task Write(StreamWriter wr)
        {
            // TODO: what should the output be here?
            await wr.WriteAsync($"{Value}");
        }
    }
    public class PoseReader
    {
        private static bool IsWhitespaceByte(char c)
        {
            return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r');
        }

        private static bool IsTokenFirstByte(char c)
        {
            return (('0' <= c && c <= '9') ||
                    ('A' <= c && c <= 'Z') ||
                    ('a' <= c && c <= 'z') ||
                    "_$!?<=>+-*/".Contains(c));
        }

        private static bool IsTokenNextByte(char c)
        {
            return IsTokenFirstByte(c) || ".@~^%&".Contains(c);
        }

        private static IExpression ParseInteger(string s, int radix)
        {
            return new Symbol(s);
        }
        private static IExpression ParseNumberOrSymbol(string s, int radix)
        {
            return new Symbol(s);
        }

        private static char? PeekByte(BinaryReader reader)
        {
            if (reader.BaseStream.Position >= reader.BaseStream.Length) return null;
            var originalPosition = reader.BaseStream.Position;
            var read = reader.ReadChar();
            reader.BaseStream.Position = originalPosition;
            return read;
        }

        private static void SkipRestOfLine(BinaryReader reader)
        {
            do
            {
                var c = reader.ReadChar();
                if (c == '\n') break;
            } while (reader.BaseStream.Position < reader.BaseStream.Length);
        }
        private static void SkipWhitespaceAndComments(BinaryReader reader)
        {
            do
            {
                var c = PeekByte(reader);
                if (c is null) break;
                if (c == ';')
                {
                    SkipRestOfLine(reader);
                }
                else if (IsWhitespaceByte(c.Value))
                {
                    reader.ReadChar();
                }
                else
                {
                    break;
                }
            } while (reader.BaseStream.Position < reader.BaseStream.Length);
        }


        static string ReadTokenAsString(BinaryReader rd)
        {
            var c1 = rd.ReadChar();

            if (!IsTokenFirstByte(c1))
            {
                throw new SyntaxErrorException("Not a token first byte");
            }

            var ans = new StringBuilder();
            ans.Append(c1);
            while (true)
            {
                var c = PeekByte(rd);
                if (c is null)
                {
                    break;
                }

                if (!IsTokenNextByte(c.Value))
                {
                    break;
                }

                c = rd.ReadChar();
                ans.Append(c);
            }

            return ans.ToString();
        }


        private static IExpression ReadSharpsign(BinaryReader rd)
        {
            var c = rd.ReadChar();
            var radix = c switch
            {
                'b' => 2,
                'o' => 8,
                'x' => 16,
                _ => 0
            };
            if (radix == 0)
            {
                throw new SyntaxErrorException("Unknown #");
            }

            var token = ReadTokenAsString(rd);
            var value = ParseInteger(token, radix);
            /*if err != nil {
			    return nil, makeSyntaxError("Cannot parse integer from token")
		    }*/
            return value;
        }

        private static IExpression ReadDelimitedList(BinaryReader rd, char endByte)
        {
            var exps = new List<IExpression>();
            while (true)
            {
                SkipWhitespaceAndComments(rd);

                var c = PeekByte(rd);
                if (c is null)
                {
                    throw new SyntaxErrorException("Unterminated list");
                }
                else if (c == endByte)
                {
                    rd.ReadChar();
                    break;
                }
                else
                {
                    var exp = Read(rd);
                    if (exp is null)
                    {
                        throw new SyntaxErrorException("Unterminated list");
                    }
                    exps.Add(exp);
                }
            }

            return new List(exps.ToArray());
        }

        private static char ReadStringEscape(BinaryReader rd, char endByte)
        {
            if (rd.BaseStream.Position >= rd.BaseStream.Length)
            {
                throw new SyntaxErrorException("Unterminated string escape");
            }
            var c = rd.ReadChar();
            switch (c)
            {
                case 'n':
                    c = '\n';
                    break;
                case 't':
                    c = '\t';
                    break;
                default:
                    {
                        if (c != '\\' && c != endByte)
                        {
                            throw new System.Data.SyntaxErrorException("Unknown string escape");
                        }
                        break;
                    }
            }
            return c;
        }

        private static string ReadDelimitedString(BinaryReader rd, char endByte)
        {
            var ans = new StringBuilder();
            while (true)
            {
                if (rd.BaseStream.Position >= rd.BaseStream.Length)
                {
                    throw new SyntaxErrorException("Unterminated string escape");
                }
                var c = rd.ReadChar();
                if (c == endByte)
                {
                    break;
                }
                else if (c == '\\')
                {
                    c = ReadStringEscape(rd, endByte);
                }

                ans.Append(c);
            }

            return ans.ToString();
        }

        private static IExpression Read(BinaryReader rd)
        {
            SkipWhitespaceAndComments(rd);
            var c = PeekByte(rd);
            if (c is null)
            {
                return null;
            }

            if (IsTokenFirstByte(c.Value))
            {
                var token = ReadTokenAsString(rd);
                return ParseNumberOrSymbol(token, default);
            }
            c = rd.ReadChar();
            switch (c)
            {
                case '#':
                    return ReadSharpsign(rd);
                case '|':
                    {
                        var s = ReadDelimitedString(rd, c.Value);
                        return new Symbol(s);
                    }
                case '"':
                    {
                        var s = ReadDelimitedString(rd, c.Value);
                        return new String(s);
                    }
                case '(':
                    return ReadDelimitedList(rd, ')');
                case ')':
                    throw new SyntaxErrorException("Stray closing parenthesis");
                default:
                    throw new SyntaxErrorException($"Unknown byte at top level: {c}");
            }
        }

        public IExpression[] ReadAll(BinaryReader rd)
        {
            var exps = new List<IExpression>();
            while (true)
            {
                var exp = Read(rd);
                if (exp is null) break;
                exps.Add(exp);
            }

            return exps.ToArray();
        }
    }

    [Serializable]
    public class SyntaxErrorException : Exception
    {
        public SyntaxErrorException()
        {
        }

        public SyntaxErrorException(string message) : base(message)
        {
        }

        public SyntaxErrorException(string message, Exception inner) : base(message, inner)
        {
        }

        protected SyntaxErrorException(
            SerializationInfo info,
            StreamingContext context) : base(info, context)
        {
        }
    }
}
