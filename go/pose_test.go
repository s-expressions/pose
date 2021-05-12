// Copyright 2021 Lassi Kortela
// SPDX-License-Identifier: ISC

package pose

import (
	"bufio"
	"bytes"
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

func testSampleEqualsOutput(t *testing.T, sample string, expected string) {
	rbuf := new(bytes.Buffer)
	_, err1 := rbuf.WriteString(sample)
	if err1 != nil {
		t.Error(err1)
		return
	}

	rd := bufio.NewReader(rbuf)
	exps, err := ReadAll(rd)
	if err != nil {
		t.Error(err)
		return
	}
	buf := new(bytes.Buffer)

	for i, exp := range exps {
		exp.Write(buf)
		if i > 0 {
			io.WriteString(buf, "\n")
		}
	}
	output := buf.String()
	if output != expected {
		t.Errorf("output %v not equal to expected %v", output, expected)
		return
	}
}

func TestSRFI(t *testing.T) {
	testSampleEqualsOutput(t, "(symbol \"value\")", "(symbol \"value\")")
	testSampleEqualsOutput(t, "; Foo", "")
	testSampleEqualsOutput(t, "  ; Bar", "")
	testSampleEqualsOutput(t, "( 1 2  (|asdo\\|aisdj| \"dfdosi dsi\"))", "(1 2 (asdo|aisdj \"dfdosi dsi\"))")
	testSampleEqualsOutput(t, "()", "()")

	testFile(t, "../examples/hello.pose")
	testFile(t, "../examples/srfi.pose")
}
