package cli

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/vida-lang/vida-lang/stdlib"
	"github.com/vida-lang/vida-lang/vida"
)

func Vida() {
	argv := os.Args
	argc := len(argv)
	switch argc {
	case 1:
		printHelp()
	default:
		switch argv[1] {
		case CIr, CIrSimple:
			showBytecode(argc, argv)
		case CRun, CRunSimple:
			runRunModule(argc, argv)
		case CDebug, CDebugSimple:
			debugModule(argc, argv)
		case CVer, CVerSimple:
			printVersion()
		case CHelp, CHelpSimple:
			printHelp()
		case CRepl:
			printNotImplementedYet("REPL")
		case CTime, CTimeSimple:
			timeModule(argc, argv)
		case CAbout, CAboutSimple:
			printAbout()
		default:
			runModule(argc, argv)
		}
	}
}

// CLI Options.
const (
	CIr          = "ir"
	CIrSimple    = "i"
	CRun         = "run"
	CRunSimple   = "r"
	CDebug       = "debug"
	CDebugSimple = "d"
	CVer         = "version"
	CVerSimple   = "v"
	CHelp        = "help"
	CHelpSimple  = "h"
	CRepl        = "repl"
	CTime        = "time"
	CTimeSimple  = "t"
	CAbout       = "about"
	CAboutSimple = "a"
)

// clearScreen is an auxiliary function that clears the screen.
func clearScreen() {
	fmt.Printf("\u001B[H")
	fmt.Printf("\u001B[2J")
}

func printVersion() {
	clearScreen()
	fmt.Printf("\n\n   %v\n   %v\n\n   Language Version %v\n   STDLib   Version %v\n   VMEngine Version %v\n\n\n", vida.LangName, vida.LangHeader, vida.LanguageVersion, vida.STDLibVersion, vida.VMEngineVersion)
}

func printAbout() {
	clearScreen()
	fmt.Printf("\n\n   %v\n   %v\n   %v\n\n\n", vida.LangName, vida.LangHeader, vida.LangWebSite)
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
