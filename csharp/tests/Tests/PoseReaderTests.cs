using System;
using System.IO;
using System.Threading.Tasks;
using Pose;
using Xunit;

namespace Tests
{
    public class PoseReaderTests
    {
        [Theory,
         InlineData("(symbol \"value\")", "(symbol \"value\")"),
         InlineData("; Foo", ""),
         InlineData("  ; Bar", ""),
         InlineData("( 1 2  (|asdo\\|aisdj| \"dfdosi dsi\"))", "(1 2 (asdo|aisdj \"dfdosi dsi\"))"),
         InlineData("()", "()"),
        ]
        public async Task Can_parse_and_stringify(string sample,string expected)
        {
            IExpression[] exps;
            {
                await using var stream = new MemoryStream();
                using var rd = new BinaryReader(stream);
                await using var w = new StreamWriter(stream);
                await w.WriteAsync(sample);
                await w.FlushAsync();
                stream.Seek(0, SeekOrigin.Begin);
                exps=new PoseReader().ReadAll(rd);
            }
            string written = null;
            {
                await using var stream = new MemoryStream();
                await using var w = new StreamWriter(stream);
                foreach (var exp in exps)
                {
                    await exp.Write(w);
                }
                await w.FlushAsync();
                stream.Seek(0, SeekOrigin.Begin);

                using var rd = new StreamReader(stream);
                written = await rd.ReadToEndAsync();
            }
            Assert.Equal(expected, written);
        }
    }
}
