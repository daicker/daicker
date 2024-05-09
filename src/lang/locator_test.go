package lang

func newRange(startLine, startColumn, endLine, endColumn int) Range {
	return Range{
		Start: Position{Line: startLine, Column: startColumn},
		End:   Position{Line: endLine, Column: endColumn},
	}
}
