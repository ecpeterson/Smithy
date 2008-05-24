RESULT = smithy
SOURCES = CamlExt.ml \
	  Resources.ml \
	  Colors.ml \
	  OrthoDrawer.ml \
	  MapTypes.ml \
	  MapFormat.ml \
	  MapDialogs.ml \
	  GeomEdit.ml \
	  DrawModeWindows.ml \
	  DrawModeEvent.ml \
	  DrawMode.ml \
	  Smithy.ml
LIBS = lablgtk
INCDIRS = +lablgtk2
OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
