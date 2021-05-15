require_relative '../pose'

def read_all(str)
    PoseReader.new(StringIO.new(str)).read_all
end

describe "sample expressions" do
    it "can parse simple expressions" do
        expect(read_all("(symbol \"value\")")).to eq [[:symbol, "value"]]
        expect(read_all("; Foo")).to eq []
        expect(read_all("  ; Bar")).to eq []
        expect(read_all("( 1 2  (|asdo\\|aisdj| \"dfdosi dsi\"))")).to eq [[:"1", :"2", [:"asdo|aisdj", "dfdosi dsi"]]]
        expect(read_all("()")).to eq [[]]
    end
end