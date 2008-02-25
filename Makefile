RESULT = smithy
SOURCES = CamlExt.ml \
	  Resources.ml \
	  Colors.ml \
	  TGALoader.ml \
	  MapTypes.ml \
	  MapFormat.ml \
	  MapDialogs.ml \
	  GeomEdit.ml \
	  GlFlatDraw.ml \
	  VisualMode.ml \
	  DrawController.ml \
	  FileDialogs.ml \
	  Smithy.ml
LIBS = lablgtk lablgl lablgtkgl
INCDIRS = +lablgtk2 +lablGL
OCAMLMAKEFILE = /usr/include/OCamlMakefile
OCAMLFLAGS = -w sy

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
