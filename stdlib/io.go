package stdlib

import (
	"fmt"
	"io"
	"io/fs"
	"os"
	"unsafe"

	"github.com/vida-lang/vida/vida"
)

// File models an open file descriptor.
type File struct {
	Value *os.File
}

// Value Interface
func (file *File) TypeName() string {
	return "File"
}

func (file *File) Description() string {
	return fmt.Sprintf("File(at:%p)", file)
}

func (file *File) Equals(other vida.Value) bool {
	if value, ok := other.(*File); ok {
		return unsafe.Pointer(file) == unsafe.Pointer(value)
	}
	return false
}

func (file *File) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, file)
}

func (file *File) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, file)
}

func (file *File) IsIterable() bool {
	return false
}

func (file *File) MakeIterator() vida.Iterator {
	return nil
}

func (file *File) IsHashable() bool {
	return false
}

func (file *File) MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func (file *File) IsValueSemantics() bool {
	return false
}

func (file *File) HasMethods() bool {
	return true
}

func (file *File) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := FileInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, file)
}

func (file *File) Clone() vida.Value {
	return file
}

var FileInterface vida.Namespace

func loadIO() vida.Importable {
	FileInterface = vida.Namespace{
		"close":       vida.GFunction{Name: "close", Value: fileClose},
		"chmod":       vida.GFunction{Name: "chmod", Value: fileChmod},
		"info":        vida.GFunction{Name: "info", Value: fileInfo},
		"name":        vida.GFunction{Name: "name", Value: fileName},
		"read":        vida.GFunction{Name: "read", Value: fileRead},
		"readAt":      vida.GFunction{Name: "readAt", Value: fileReadAt},
		"seek":        vida.GFunction{Name: "seek", Value: fileSeek},
		"sync":        vida.GFunction{Name: "seek", Value: fileSync},
		"write":       vida.GFunction{Name: "write", Value: fileWrite},
		"writeAt":     vida.GFunction{Name: "writeAt", Value: fileWriteAt},
		"writeString": vida.GFunction{Name: "writeString", Value: fileWriteString},
		"truncate":    vida.GFunction{Name: "truncate", Value: fileTruncate},
	}
	gmodule := vida.GModule{Name: "io", Namespace: vida.Namespace{
		"newFile":       vida.GFunction{Name: "newFile", Value: fileCreateFile},
		"readFile":      vida.GFunction{Name: "readFile", Value: fileReadFile},
		"openFile":      vida.GFunction{Name: "openFile", Value: fileOpenFile},
		"readAll":       vida.GFunction{Name: "readAll", Value: ioReadAll},
		"remove":        vida.GFunction{Name: "remove", Value: ioRemove},
		"removeAll":     vida.GFunction{Name: "removeAll", Value: ioRemoveAll},
		"rename":        vida.GFunction{Name: "rename", Value: ioRename},
		"tempDir":       vida.GFunction{Name: "tempDir", Value: ioTempDir},
		"makeDir":       vida.GFunction{Name: "makeDir", Value: ioMakeDir},
		"r":             vida.Int(os.O_RDONLY),
		"w":             vida.Int(os.O_WRONLY),
		"rw":            vida.Int(os.O_RDWR),
		"a":             vida.Int(os.O_APPEND),
		"c":             vida.Int(os.O_CREATE),
		"t":             vida.Int(os.O_TRUNC),
		"seekStart":     vida.Int(io.SeekStart),
		"seekCurrent":   vida.Int(io.SeekCurrent),
		"seekEnd":       vida.Int(io.SeekEnd),
		"stdin":         &File{Value: os.Stdin},
		"stdout":        &File{Value: os.Stdout},
		"stderr":        &File{Value: os.Stderr},
		"modeDir":       vida.Int(os.ModeDir),
		"modeAppend":    vida.Int(os.ModeAppend),
		"modeExclusive": vida.Int(os.ModeExclusive),
		"modeTemporary": vida.Int(os.ModeTemporary),
		"modeSymlink":   vida.Int(os.ModeSymlink),
		"modeDevice":    vida.Int(os.ModeDevice),
		"modeNamedPipe": vida.Int(os.ModeNamedPipe),
		"modeSocket":    vida.Int(os.ModeSocket),
		"modePerm":      vida.Int(os.ModePerm),
	}}
	return gmodule
}

func fileCreateFile(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if str, ok := args[0].(*vida.String); ok {
			if file, err := os.Create(str.Value); err == nil {
				return vida.Result{Value: &File{Value: file}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileReadFile(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if str, ok := args[0].(*vida.String); ok {
			if content, err := os.ReadFile(str.Value); err == nil {
				return vida.Result{Value: &vida.Bytes{Value: content}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileOpenFile(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if str, ok := args[0].(*vida.String); ok {
			if flag, ok := args[1].(vida.Int); ok {
				if file, err := os.OpenFile(str.Value, int(flag), os.ModePerm); err == nil {
					return vida.Result{Value: &File{Value: file}, Error: vida.NilValue}, nil
				} else {
					return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[1].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func fileClose(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if err := args[0].(*File).Value.Close(); err == nil {
			return vida.NilValue, nil
		} else {
			return nil, err
		}
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func fileChmod(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		file := args[0].(*File)
		if mode, ok := args[1].(vida.Int); ok {
			if err := file.Value.Chmod(os.FileMode(mode)); err == nil {
				return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func fileName(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		return &vida.String{Value: args[0].(*File).Value.Name()}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func fileRead(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		file := args[0].(*File)
		if count, ok := args[1].(vida.IntegerNumber); ok {
			b := &vida.Bytes{Value: make([]byte, count.ToInt())}
			if _, e := file.Value.Read(b.Value); e == nil {
				return b, nil
			} else {
				return nil, e
			}
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileSeek(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		file := args[0].(*File)
		if offset, ok := args[1].(vida.IntegerNumber); ok {
			if whence, ok := args[2].(vida.IntegerNumber); ok {
				if ret, e := file.Value.Seek(int64(offset.ToInt()), int(whence.ToInt())); e == nil {
					return vida.Int(ret), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[2].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 3, len(args))
}

func fileSync(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if err := args[0].(*File).Value.Sync(); err == nil {
			return vida.NilValue, nil
		} else {
			return nil, err
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func fileWrite(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		file := args[0].(*File)
		if data, ok := args[1].(*vida.Bytes); ok {
			if n, e := file.Value.Write(data.Value); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileWriteAt(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		file := args[0].(*File)
		if data, ok := args[1].(*vida.Bytes); ok {
			if offset, ok := args[2].(vida.IntegerNumber); ok {
				if n, e := file.Value.WriteAt(data.Value, int64(offset.ToInt())); e == nil {
					return vida.Int(n), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[2].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Bytes", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func fileReadAt(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		file := args[0].(*File)
		if data, ok := args[1].(*vida.Bytes); ok {
			if offset, ok := args[2].(vida.IntegerNumber); ok {
				if n, e := file.Value.ReadAt(data.Value, int64(offset.ToInt())); e == nil {
					return vida.Int(n), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[2].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Bytes", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func fileWriteString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		file := args[0].(*File)
		if data, ok := args[1].(*vida.String); ok {
			if n, e := file.Value.WriteString(data.Value); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileTruncate(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		file := args[0].(*File)
		if size, ok := args[1].(vida.IntegerNumber); ok {
			if err := file.Value.Truncate(int64(size.ToInt())); err == nil {
				return vida.NilValue, nil
			} else {
				return nil, err
			}
		}
		return nil, vida.ExpectedTypeAndGotOtherType("Integer", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func fileInfo(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		file := args[0].(*File)
		if fileInfo, err := file.Value.Stat(); err == nil {
			info := vida.Record{Properties: make(vida.Namespace)}
			info.Properties["name"] = &vida.String{Value: fileInfo.Name()}
			info.Properties["size"] = vida.Int(fileInfo.Size())
			info.Properties["mode"] = vida.Int(fileInfo.Mode())
			info.Properties["modTime"] = Time{Value: fileInfo.ModTime()}
			info.Properties["isDir"] = vida.Bool(fileInfo.IsDir())
			return info, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func ioReadAll(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case *File:
			if b, err := io.ReadAll(value.Value); err == nil {
				return vida.Result{Value: &vida.Bytes{Value: b}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		case *BytesBuffer:
			if b, err := io.ReadAll(&value.Buffer); err == nil {
				return vida.Result{Value: &vida.Bytes{Value: b}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		default:
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: "expected a value of type File of BytesBuffer"}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func ioRemove(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if str, ok := args[0].(*vida.String); ok {
			if err := os.Remove(str.Value); err == nil {
				return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func ioRemoveAll(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if str, ok := args[0].(*vida.String); ok {
			if err := os.RemoveAll(str.Value); err == nil {
				return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func ioRename(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if oldPath, ok := args[0].(*vida.String); ok {
			if newPath, ok := args[1].(*vida.String); ok {
				if err := os.Rename(oldPath.Value, newPath.Value); err == nil {
					return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
				} else {
					return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("String", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("String", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func ioTempDir(args ...vida.Value) (vida.Value, error) {
	if len(args) == 0 {
		return &vida.String{Value: os.TempDir()}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func ioMakeDir(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if name, ok := args[0].(*vida.String); ok {
			if fmod, ok := args[1].(vida.Int); ok {
				if err := os.Mkdir(name.Value, fs.FileMode(fmod)); err == nil {
					return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
				} else {
					return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Int", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("String", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}
