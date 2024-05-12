package lang

import (
	"errors"
	"fmt"
	"slices"

	"github.com/macrat/simplexer"
	"github.com/tliron/commonlog"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"github.com/tliron/glsp/server"

	_ "github.com/tliron/commonlog/simple"
)

const lsName = "Dacrane Language Server"

var (
	version string = "0.0.1"
	handler protocol.Handler
)

var Files = map[string]string{}
var Modules = map[string]Module{}

func LanguageServerStart() {
	// This increases logging verbosity (optional)
	commonlog.Configure(2, nil)

	handler = protocol.Handler{
		Initialize:                     initialize,
		Initialized:                    initialized,
		Shutdown:                       shutdown,
		SetTrace:                       setTrace,
		TextDocumentCompletion:         TextDocumentCompletion,
		TextDocumentSemanticTokensFull: TextDocumentSemanticTokensFull,
		TextDocumentDidOpen:            TextDocumentDidOpen,
		TextDocumentDidChange:          TextDocumentDidChange,
		TextDocumentDidSave:            TextDocumentDidSave,
	}

	server := server.NewServer(&handler, lsName, true)

	server.RunStdio()
}

func initialize(context *glsp.Context, params *protocol.InitializeParams) (any, error) {
	commonlog.NewInfoMessage(0, "Initializing server...")

	capabilities := handler.CreateServerCapabilities()

	capabilities.TextDocumentSync = protocol.TextDocumentSyncKindFull
	capabilities.CompletionProvider = &protocol.CompletionOptions{}
	capabilities.SemanticTokensProvider = &protocol.SemanticTokensOptions{
		Legend: protocol.SemanticTokensLegend{
			TokenTypes:     tokenTypes,
			TokenModifiers: []string{},
		},
		Full: protocol.True,
	}

	return protocol.InitializeResult{
		Capabilities: capabilities,
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    lsName,
			Version: &version,
		},
	}, nil
}

func initialized(context *glsp.Context, params *protocol.InitializedParams) error {
	return nil
}

func shutdown(context *glsp.Context) error {
	protocol.SetTraceValue(protocol.TraceValueOff)
	return nil
}

func setTrace(context *glsp.Context, params *protocol.SetTraceParams) error {
	protocol.SetTraceValue(params.Value)
	return nil
}

func TextDocumentCompletion(context *glsp.Context, params *protocol.CompletionParams) (interface{}, error) {
	var completionItems []protocol.CompletionItem

	operator := protocol.CompletionItemKindOperator
	for _, f := range FixtureFunctions {
		completionItems = append(completionItems, protocol.CompletionItem{
			Label: f.Name,
			Kind:  &operator,
		})
	}
	variable := protocol.CompletionItemKindVariable
	m := Modules[params.TextDocument.URI]
	for _, v := range m.Vars {
		t, err := Infer(v.Expr, m)
		if err != nil {
			continue
		}
		ts := t.String()
		completionItems = append(completionItems, protocol.CompletionItem{
			Label:  v.Name,
			Kind:   &variable,
			Detail: &ts,
		})
	}

	return completionItems, nil
}

func TextDocumentDidOpen(context *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	text := params.TextDocument.Text
	uri := params.TextDocument.URI
	Files[uri] = text
	SendNotifications(context, uri, text)
	return nil
}

func TextDocumentDidChange(context *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	text := params.ContentChanges[0].(protocol.TextDocumentContentChangeEventWhole).Text
	uri := params.TextDocument.URI
	Files[uri] = text
	SendNotifications(context, uri, text)
	return nil
}

func TextDocumentDidSave(context *glsp.Context, params *protocol.DidSaveTextDocumentParams) error {
	return nil
}

func SendNotifications(context *glsp.Context, uri string, text string) {
	var codeErr *CodeError
	codeErrors := []CodeError{}

	tokens, err := Lex(text)
	if errors.As(err, &codeErr) {
		codeErrors = append(codeErrors, *codeErr)
		SendCodeError(context, uri, codeErrors)
		return
	}

	m, err := Parse(tokens)
	if errors.As(err, &codeErr) {
		codeErrors = append(codeErrors, *codeErr)
		SendCodeError(context, uri, codeErrors)
		return
	}
	Modules[uri] = m

	for _, v := range m.Vars {
		_, err := Infer(v.Expr, m)
		if errors.As(err, &codeErr) {
			codeErrors = append(codeErrors, *codeErr)
		}
	}

	SendCodeError(context, uri, codeErrors)
}

func TextDocumentSemanticTokensFull(context *glsp.Context, params *protocol.SemanticTokensParams) (*protocol.SemanticTokens, error) {
	text := Files[params.TextDocument.URI]

	tokens, err := Lex(text)
	if err != nil {
		return nil, nil
	}

	return CreateSemanticTokens(tokens)
}

var tokenTypes = []string{"number", "string", "operator", "variable"}

var tokenMap = map[int]string{
	IDENTIFIER: "variable",
	INTEGER:    "number",
	STRING:     "string",
	ADD:        "operator",
	ASSIGN:     "operator",
	COMMA:      "operator",
	ARROW:      "operator",
	BSLASH:     "operator",
	LBRACKET:   "operator",
	RBRACKET:   "operator",
}

func TokenKindNumber(token *simplexer.Token) (uint32, error) {
	kind, ok := tokenMap[int(token.Type.GetID())]
	if !ok {
		return 99, fmt.Errorf("cannot mapping token kind: token id (%d)", token.Type.GetID())
	}
	i := slices.Index(tokenTypes, kind)
	if i == -1 {
		return 99, fmt.Errorf("cannot mapping token kind: token id (%d)", token.Type.GetID())
	}
	return uint32(i), nil
}

func CreateSemanticTokens(tokens []*simplexer.Token) (*protocol.SemanticTokens, error) {
	data := []uint32{}
	if len(tokens) == 0 {
		return &protocol.SemanticTokens{
			Data: data,
		}, nil
	}
	previousToken := tokens[0]
	for _, t := range tokens {
		tokenKindNumber, err := TokenKindNumber(t)
		if err != nil {
			return nil, err
		}
		dColumn := 0
		if t.Position.Line == previousToken.Position.Line {
			dColumn = t.Position.Column - previousToken.Position.Column
		}

		data = append(data,
			uint32(t.Position.Line-previousToken.Position.Line),
			uint32(dColumn),
			uint32(len(t.Literal)),
			tokenKindNumber,
			0, //
		)
		previousToken = t
	}
	return &protocol.SemanticTokens{
		Data: data,
	}, nil
}

func SendCodeError(context *glsp.Context, uri string, codeErrors []CodeError) {
	diagnostics := []protocol.Diagnostic{}
	for _, codeError := range codeErrors {
		diagnostics = append(diagnostics, protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: uint32(codeError.Range.Start.Line), Character: uint32(codeError.Range.Start.Column)},
				End:   protocol.Position{Line: uint32(codeError.Range.End.Line), Character: uint32(codeError.Range.End.Column)},
			},
			Message: codeError.Message,
		})
	}

	context.Notify("textDocument/publishDiagnostics", protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	})
}

func SendNoError(context *glsp.Context, uri string) {
	context.Notify("textDocument/publishDiagnostics", protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: []protocol.Diagnostic{},
	})
}
