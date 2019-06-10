DAMLC := $(HOME)/damlc-dev
ENGINE := ../target/release/rusty-engine

DAMLS := Queens.daml Sort.daml
DALFS := $(DAMLS:.daml=.dalf)
TESTS := $(DALFS:.dalf=.test)

.PHONY: all test clean $(ENGINE)

.SILENT: test

all: $(DALFS)

$(ENGINE):
	cargo build --release

test: $(TESTS)
	$(foreach TEST,$(TESTS),echo "=========="; cat $(TEST);)

%.dalf: %.daml
	$(DAMLC) compile --daml-1.2 -o $@ $<

%.test: %.dalf $(ENGINE)
	$(ENGINE) $< > $@

clean:
	rm -f $(DALFS)
