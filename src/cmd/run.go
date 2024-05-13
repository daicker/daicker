package cmd

import (
	"errors"
	"fmt"
	"os"

	"github.com/daicker/daicker/src/lang"
	"github.com/spf13/cobra"
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run function",
	Long:  `Run function`,
	Args: func(cmd *cobra.Command, args []string) error {
		if len(args) < 1 {
			return errors.New("requires file name")
		}
		return nil
	},
	Run: func(cmd *cobra.Command, args []string) {
		file := args[0]
		src, err := os.ReadFile(file)
		if err != nil {
			panic(err)
		}

		tokens, err := lang.Lex(string(src))
		if err != nil {
			panic(err)
		}

		mod, err := lang.Parse(tokens)
		if err != nil {
			panic(err)
		}

		res, err := lang.Eval(function, mod)
		if err != nil {
			panic(err)
		}

		fmt.Println(res)
	},
}

var function string

func init() {
	rootCmd.AddCommand(runCmd)
	runCmd.Flags().StringVarP(&function, "function", "f", "main", "Daicker target function")
}
