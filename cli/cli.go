package cli

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/vida-lang/vida/stdlib"
	"github.com/vida-lang/vida/vida"
)

func Vida() {
	argv := os.Args
	argc := len(argv)
	switch argc {
	case 1:
		printHelp()
	default:
		switch argv[1] {
		case CIr, CIrShort:
			showBytecode(argc, argv)
		case CRun, CRunShort:
			runRunModule(argc, argv)
		case CDebug, CDebugShort:
			debugModule(argc, argv)
		case CVer, CVerShort:
			printVersion()
		case CHelp, CHelpShort:
			printHelp()
		case CRepl:
			printNotImplementedYet("REPL")
		case CTime, CTimeShort:
			timeModule(argc, argv)
		case CAbout, CAboutShort:
			printAbout()
		default:
			runModule(argc, argv)
		}
	}
}

// CLI Options.
const (
	CIr         = "ir"
	CIrShort    = "i"
	CRun        = "run"
	CRunShort   = "r"
	CDebug      = "debug"
	CDebugShort = "d"
	CVer        = "version"
	CVerShort   = "v"
	CHelp       = "help"
	CHelpShort  = "h"
	CRepl       = "repl"
	CTime       = "time"
	CTimeShort  = "t"
	CAbout      = "about"
	CAboutShort = "a"
)

// clearScreen is an auxiliary function that clears the screen.
func clearScreen() {
	fmt.Printf("\u001B[H")
	fmt.Printf("\u001B[2J")
}

func printVersion() {
	clearScreen()
	fmt.Printf("\n\n   %v\n   %v\n\n   Language Version %v\n   STDLib   Version %v\n   VMEngine Version %v\n\n\n", vida.LangName, vida.LangHeader, vida.LangVersion, vida.STDLibVersion, vida.VMEngineVersion)
}

func printAbout() {
	clearScreen()
	fmt.Printf("\n\n   %v\n   Made with ❤️  and Go.\n   %v\n   %v\n   hello@vida-lang.org\n\n\n", vida.LangName, vida.LangHeader, vida.LangWebSite)
}

func printNotImplementedYet(functionality string) {
	clearScreen()
	fmt.Printf("\n\n\n   %v\n   %v option\n   Not Implemented Yet\n\n\n", vida.LangName, functionality)
}

func printHelp() {
	clearScreen()
	fmt.Printf("\n\n   %v\n   %v\n\n   Command Line Interface Usage:\n   vida [option]? <mod-name>.vida [args...]?\n\n\n", vida.LangName, vida.LangWebSite)
	fmt.Printf("   i/ir       Prints the itermediate representation of a module\n")
	fmt.Printf("   r/run      Runs the module given as argument\n")
	fmt.Printf("   t/time     Prints the time elapsed after running a module\n")
	fmt.Printf("   d/debug    Runs a module in step by step execution mode\n")
	fmt.Printf("   v/version  Shows the language, stdlib and VM versions\n")
	fmt.Printf("   a/about    Prints basic information about the language\n")
	fmt.Printf("   repl       Starts the interpreter in mode REPL\n")
	fmt.Printf("   h/help     Shows this message\n\n\n")
	flag.PrintDefaults()
}

// runModule runs the given modules.
func runModule(argc int, argv []string) {
	if argc >= 2 {
		modIndex := 1
		if strings.HasSuffix(argv[modIndex], vida.ModuleExtension) {
			if abspath, pathError := filepath.Abs(argv[modIndex]); pathError == nil {
				if input, err := vida.LoadModule(abspath); err == nil {
					vida.RunModule(input, abspath, stdlib.Stdlib)
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				fmt.Printf("%v", pathError)
				os.Exit(0)
			}
		} else {
			info := fmt.Sprintf("\n\nThe file '%v' is not a Vida module\n\n\n", argv[modIndex])
			fmt.Printf("%v", fmt.Errorf(info))
			os.Exit(0)
		}
	}
}

func runRunModule(argc int, argv []string) {
	if argc >= 3 {
		modIndex := 2
		if strings.HasSuffix(argv[modIndex], vida.ModuleExtension) {
			if abspath, pathError := filepath.Abs(argv[modIndex]); pathError == nil {
				if input, err := vida.LoadModule(abspath); err == nil {
					vida.RunModule(input, abspath, stdlib.Stdlib)
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				fmt.Printf("%v", pathError)
				os.Exit(0)
			}
		} else {
			info := fmt.Sprintf("\n\nThe file '%v' is not a Vida module\n\n\n", argv[modIndex])
			fmt.Printf("%v", fmt.Errorf(info))
			os.Exit(0)
		}
	}
}

func showBytecode(argc int, argv []string) {
	if argc >= 3 {
		modIndex := 2
		if strings.HasSuffix(argv[modIndex], vida.ModuleExtension) {
			if abspath, pathError := filepath.Abs(argv[modIndex]); pathError == nil {
				if input, err := vida.LoadModule(abspath); err == nil {
					clearScreen()
					vida.PrintCode(input, abspath)
					println()
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				fmt.Printf("%v", pathError)
				os.Exit(0)
			}
		} else {
			info := fmt.Sprintf("\n\nThe file '%v' is not a Vida module\n\n\n", argv[modIndex])
			fmt.Printf("%v", fmt.Errorf(info))
			os.Exit(0)
		}
	}
}

func debugModule(argc int, argv []string) {
	if argc >= 3 {
		modIndex := 2
		if strings.HasSuffix(argv[modIndex], vida.ModuleExtension) {
			if abspath, pathError := filepath.Abs(argv[modIndex]); pathError == nil {
				if input, err := vida.LoadModule(abspath); err == nil {
					clearScreen()
					vida.DebugModule(input, abspath, stdlib.Stdlib)
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				fmt.Printf("%v", pathError)
				os.Exit(0)
			}
		} else {
			info := fmt.Sprintf("\n\nThe file '%v' is not a Vida module\n\n\n", argv[modIndex])
			fmt.Printf("%v", fmt.Errorf(info))
			os.Exit(0)
		}
	}
}

func timeModule(argc int, argv []string) {
	if argc >= 3 {
		modIndex := 2
		if strings.HasSuffix(argv[modIndex], vida.ModuleExtension) {
			if abspath, pathError := filepath.Abs(argv[modIndex]); pathError == nil {
				if input, err := vida.LoadModule(abspath); err == nil {
					vida.TimeModule(input, abspath, stdlib.Stdlib)
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				fmt.Printf("%v", pathError)
				os.Exit(0)
			}
		} else {
			info := fmt.Sprintf("\n\nThe file '%v' is not a Vida module\n\n\n", argv[modIndex])
			fmt.Printf("%v", fmt.Errorf(info))
			os.Exit(0)
		}
	}
}

func runRepl() {
	prompt, userPrompt, defaultPrompt := "", "", "❯"
	prompt = defaultPrompt
	var code string
	scanner := bufio.NewScanner(os.Stdin)
	repl, showCode := vida.NewRepl("repl", stdlib.Stdlib), false
	clearScreen()
	fmt.Printf("\n\n\n   Welcome to %v\n   Version %v\n   Type .help for assistance.\n\n\n", vida.LangName, vida.LangVersion)
replLoop:
	for {
		fmt.Printf("%2v ", prompt)
		if scanner.Scan() {
			code = scanner.Text()
			switch code {
			case ".q", ".quit", ".exit":
				break replLoop
			case ".h", ".help":
				replHelp()
			case ".c", ".clear":
				clearScreen()
				println()
			case ".s", ".show":
				if showCode {
					showCode = false
					fmt.Printf("\n   Show code is off\n\n")
				} else {
					showCode = true
					fmt.Printf("\n   Show code is on\n\n")
				}
			case ".dp", ".defaultPrompt":
				prompt = defaultPrompt
				println()
			case ".sp", ".setPrompt":
				fmt.Print("   Enter the new prompt : ")
				if scanner.Scan() {
					userPrompt = scanner.Text()
					prompt = userPrompt
					println()
				} else {
					break replLoop
				}
			default:
				println()
				if err := repl.Eval(code, showCode); err != nil {
					fmt.Printf("\n\n   Error\n   %v\n\n   Code\n   %v\n\n\n", err, code)
				} else {
					println()
				}
			}
		} else {
			break replLoop
		}
	}
	fmt.Printf("\n\n   Leaving REPL.\n\n\n")
}

func replHelp() {
	clearScreen()
	fmt.Printf("\n\n\n   Vida REPL Help 🌻\n\n")
	fmt.Printf("   Type .command [args]?\n\n")
	fmt.Println("   [h help] Shows this message")
	fmt.Println("   [q quit exit] Quits the REPL")
	fmt.Println("   [s show] Sets on printing readable machine code")
	fmt.Println("   [e env] Shows the current context")
	fmt.Println("   [sp setPrompt] Sets a new prompt")
	fmt.Println("   [dp defaultPrompt] Sets prompt to its default value")
	fmt.Println("   [it] Shows the last computed expression")
	fmt.Println("   [c clear] Clears the terminal")
	fmt.Printf("\n\n")
}
