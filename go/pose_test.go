package pose

import (
	"bufio"
	"io"
	"os"
	"testing"
)

func testFile(t *testing.T, filename string) {
	file, err := os.Open(filename)
	if err != nil {
		t.Error(err)
	}
	defer file.Close()
	rd := bufio.NewReader(file)
	exps, err := ReadAll(rd)
	if err != nil {
		t.Error(err)
		return
	}
	for _, exp := range exps {
		exp.Write(os.Stdout)
		io.WriteString(os.Stdout, "\n")
	}
}

func TestSRFI(t *testing.T) {
	testFile(t, "../examples/hello.pose")
	testFile(t, "../examples/srfi.pose")
}
