// Copyright 2021 Lassi Kortela
// SPDX-License-Identifier: ISC

package pose

import (
	"bufio"
	"fmt"
	"io"
	"math/big"
	"strconv"
	"strings"
)

type Exp interface {
	Write(wr io.Writer) error
}

type List struct {
	value []Exp
}

type Symbol struct {
	value string
}

type String struct {
	value string
}

type Float64 struct {
	value float64
}

type FixInt struct {
	value int
}

type BigInt struct {
	value big.Int
}

func (exp List) Write(wr io.Writer) error {
	n := len(exp.value)
	if n == 0 {
		_, err := io.WriteString(wr, "()")
		return err
	}
	prefix := "("
	for i := 0; i < n; i += 1 {
		_, err := io.WriteString(wr, prefix)
		if err != nil {
			return err
		}
		err = exp.value[i].Write(wr)
		if err != nil {
			return err
		}
		prefix = " "
	}
	_, err := io.WriteString(wr, ")")
	if err != nil {
		return err
	}
	return nil
}

func (exp Symbol) Write(wr io.Writer) error {
	_, err := io.WriteString(wr, exp.value)
	return err
}

func (exp String) Write(wr io.Writer) error {
	_, err := io.WriteString(wr, "\""+exp.value+"\"")
	return err
}

func (exp Float64) Write(wr io.Writer) error {
	_, err := io.WriteString(wr, strconv.FormatFloat(exp.value, 'f', 5, 64))
	return err
}

func (exp FixInt) Write(wr io.Writer) error {
	_, err := io.WriteString(wr, strconv.Itoa(exp.value))
	return err
}

func (exp BigInt) Write(wr io.Writer) error {
	_, err := io.WriteString(wr, exp.value.String())
	return err
}

type SyntaxError struct {
	msg string
}

func (e SyntaxError) Error() string {
	return e.msg
}

func makeSyntaxError(msg string) SyntaxError {
	return SyntaxError{msg: msg}
}

func isWhitespaceByte(c byte) bool {
	return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r')
}

func isTokenFirstByte(c byte) bool {
	return (('0' <= c && c <= '9') ||
		('A' <= c && c <= 'Z') ||
		('a' <= c && c <= 'z') ||
		strings.ContainsRune("_$!?<=>+-*/", rune(c)))
}

func isTokenNextByte(c byte) bool {
	return isTokenFirstByte(c) || strings.ContainsRune(".@~^%&", rune(c))
}

func parseInteger(s string, radix int) (Exp, error) {
	return Symbol{value: s}, nil
}

func parseNumberOrSymbol(s string) (Exp, error) {
	return Symbol{value: s}, nil
}

func peekByte(rd *bufio.Reader) (byte, error) {
	c, err := rd.ReadByte()
	if err != nil {
		return 0, err
	}
	err = rd.UnreadByte()
	if err != nil {
		return 0, err
	}
	return c, nil
}

func skipRestOfLine(rd *bufio.Reader) error {
	for {
		c, err := rd.ReadByte()
		if err == io.EOF {
			break
		} else if err != nil {
			return err
		} else if c == '\n' {
			break
		}
	}
	return nil
}

func skipWhitespaceAndComments(rd *bufio.Reader) error {
	for {
		c, err := peekByte(rd)
		if err == io.EOF {
			break
		} else if err != nil {
			return err
		} else if c == ';' {
			err = skipRestOfLine(rd)
			if err != nil {
				return err
			}
		} else if isWhitespaceByte(c) {
			_, err := rd.ReadByte()
			if err != nil {
				return err
			}
		} else {
			break
		}
	}
	return nil
}

func readTokenAsString(rd *bufio.Reader) (string, error) {
	c, err := rd.ReadByte()
	if err != nil {
		return "", err
	}
	if !isTokenFirstByte(c) {
		return "", makeSyntaxError("Not a token first byte")
	}
	var ans strings.Builder
	ans.WriteByte(c)
	for {
		c, err := peekByte(rd)
		if err == io.EOF {
			break
		} else if err != nil {
			return "", err
		} else if !isTokenNextByte(c) {
			break
		}
		c, err = rd.ReadByte()
		if err != nil {
			return "", err
		}
		ans.WriteByte(c)
	}
	return ans.String(), nil
}

func readSharpsign(rd *bufio.Reader) (Exp, error) {
	c, err := rd.ReadByte()
	radix := 0
	switch c {
	case 'b':
		radix = 2
	case 'o':
		radix = 8
	case 'x':
		radix = 16
	}
	if radix == 0 {
		return nil, makeSyntaxError("Unknown #")
	}
	token, err := readTokenAsString(rd)
	if err != nil {
		return nil, err
	}
	value, err := parseInteger(token, radix)
	if err != nil {
		return nil, makeSyntaxError("Cannot parse integer from token")
	}
	return value, nil
}

func readDelimitedList(rd *bufio.Reader, endByte byte) (Exp, error) {
	exps := []Exp{}
	for {
		err := skipWhitespaceAndComments(rd)
		if err != nil {
			return nil, err
		}
		c, err := peekByte(rd)
		if err == io.EOF {
			return nil, makeSyntaxError("Unterminated list")
		} else if err != nil {
			return nil, err
		} else if c == endByte {
			_, err := rd.ReadByte()
			if err != nil {
				return nil, err
			}
			break
		} else {
			exp, err := Read(rd)
			if err != nil {
				return nil, makeSyntaxError("Unterminated list")
			}
			exps = append(exps, exp)
		}
	}
	return List{value: exps}, nil
}

func readStringEscape(rd *bufio.Reader, endByte byte) (byte, error) {
	c, err := rd.ReadByte()
	if err == io.EOF {
		return 0, makeSyntaxError("Unterminated string escape")
	} else if err != nil {
		return 0, err
	} else if c == 'n' {
		c = '\n'
	} else if c == 't' {
		c = '\t'
	} else if c != '\\' && c != endByte {
		return 0, makeSyntaxError("Unknown string escape")
	}
	return c, nil
}

func readDelimitedString(rd *bufio.Reader, endByte byte) (string, error) {
	var ans strings.Builder
	for {
		c, err := rd.ReadByte()
		if c == endByte {
			break
		} else if err == io.EOF {
			return "", makeSyntaxError("Unterminated string")
		} else if err != nil {
			return "", err
		} else if c == '\\' {
			var err error
			c, err = readStringEscape(rd, endByte)
			if err != nil {
				return "", err
			}
		}
		ans.WriteByte(c)
	}
	return ans.String(), nil
}

func Read(rd *bufio.Reader) (Exp, error) {
	err := skipWhitespaceAndComments(rd)
	if err != nil {
		return nil, err
	}
	c, err := peekByte(rd)
	if err != nil {
		return nil, err
	} else if isTokenFirstByte(c) {
		token, err := readTokenAsString(rd)
		if err != nil {
			return nil, err
		}
		return parseNumberOrSymbol(token)
	} else {
		c, err := rd.ReadByte()
		if err != nil {
			return nil, err
		} else if c == '#' {
			return readSharpsign(rd)
		} else if c == '|' {
			s, err := readDelimitedString(rd, c)
			if err != nil {
				return nil, err
			}
			return Symbol{value: s}, nil
		} else if c == '"' {
			s, err := readDelimitedString(rd, c)
			if err != nil {
				return nil, err
			}
			return String{value: s}, nil
		} else if c == '(' {
			return readDelimitedList(rd, ')')
		} else if c == ')' {
			return nil, makeSyntaxError("Stray closing parenthesis")
		} else {
			return nil, makeSyntaxError(
				fmt.Sprintf("Unknown byte at top level: 0x%02x", c))
		}
	}
}

func ReadAll(rd *bufio.Reader) ([]Exp, error) {
	exps := []Exp{}
	for {
		exp, err := Read(rd)
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		}
		exps = append(exps, exp)
	}
	return exps, nil
}
