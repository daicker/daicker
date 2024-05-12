package lang

import (
	"errors"
	"strings"

	"github.com/macrat/simplexer"
)

type TokenIterationLexer struct {
	tokens    []*simplexer.Token
	lastIndex int
	lastToken *simplexer.Token
	error     error
	result    Module
}

func NewTokenIterationLexer(tokens []*simplexer.Token) *TokenIterationLexer {
	return &TokenIterationLexer{
		tokens:    tokens,
		lastIndex: -1,
		lastToken: nil,
	}
}

func (l *TokenIterationLexer) Lex(lval *yySymType) int {
	l.lastIndex++
	if l.lastIndex > len(l.tokens)-1 {
		return -1
	}
	token := l.tokens[l.lastIndex]
	lval.token = token
	l.lastToken = token

	return int(token.Type.GetID())
}

func (l *TokenIterationLexer) Error(e string) {
	if l.lastToken == nil {
		l.error = NewCodeError(Range{
			Start: Position{Line: 0, Column: 0},
			End:   Position{Line: 0, Column: 0},
		}, e)
	}
	l.error = NewCodeError(NewRangeFromToken(*l.lastToken), e)
}

func Lex(code string) ([]*simplexer.Token, error) {
	reader := strings.NewReader(code)
	l := simplexer.NewLexer(reader)

	l.TokenTypes = []simplexer.TokenType{
		simplexer.NewRegexpTokenType(MODULE, "module"),
		simplexer.NewRegexpTokenType(IDENTIFIER, `[a-zA-Z_][a-zA-Z0-9_]*`),
		simplexer.NewRegexpTokenType(INTEGER, `[0-9]+`),
		simplexer.NewRegexpTokenType(STRING, `"([^"]*)"`),
		simplexer.NewRegexpTokenType(ADD, `\+`),
		simplexer.NewRegexpTokenType(ARROW, `=>`),
		simplexer.NewRegexpTokenType(ASSIGN, `=`),
		simplexer.NewRegexpTokenType(COMMA, `,`),
		simplexer.NewRegexpTokenType(BSLASH, `\\`),
		simplexer.NewRegexpTokenType(LBRACKET, `\(`),
		simplexer.NewRegexpTokenType(RBRACKET, `\)`),
	}

	tokens := []*simplexer.Token{}

	for {
		token, err := l.Scan()

		if err != nil {
			var unknownTokenError simplexer.UnknownTokenError
			if errors.As(err, &unknownTokenError) {
				return nil, NewCodeError(
					NewRangeFromToken(simplexer.Token{
						Literal:  unknownTokenError.Literal,
						Position: unknownTokenError.Position,
					}),
					err.Error(),
				)
			} else {
				return nil, err
			}
		}

		if token == nil {
			break
		}

		tokens = append(tokens, token)
	}

	if len(tokens) == 0 {
		return nil, &CodeError{
			Range: Range{
				Start: Position{Line: 0, Column: 0},
				End:   Position{Line: 0, Column: 0},
			},
			Message: "no token",
		}
	}

	return tokens, nil
}
