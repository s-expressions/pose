// Copyright 2021 Lassi Kortela
// SPDX-License-Identifier: ISC

package pose

import (
	"bufio"
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"strings"
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
	_ = exps
}

func testFileMatches(t *testing.T, filename string, filenameExpected string) {
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
	buf := new(bytes.Buffer)

	for i, exp := range exps {
		exp.Write(buf)
		if i > 0 {
			io.WriteString(buf, "\n")
		}
	}
	output := strings.Trim(buf.String(), "\r\n")
	bytesExpected, err := ioutil.ReadFile(filenameExpected)
	expected := strings.Trim(string(bytesExpected), "\r\n")
	if err != nil {
		t.Fatal(err)
		return
	}
	if output != expected {
		t.Errorf("output %v not equal to expected %v", output, expected)
		return
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
		t.Errorf("could not read sample %s", sample)
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

	files, err := ioutil.ReadDir("../examples/test")
	if err != nil {
		t.Fatal(err)
		return
	}

	for _, file := range files {
		if !file.IsDir() && strings.HasSuffix(file.Name(), ".pose") {
			testFileMatches(t, "../examples/test/"+file.Name(), "../examples/test/"+(strings.Replace(file.Name(), ".pose", ".result", 1)))
		}
	}
}
