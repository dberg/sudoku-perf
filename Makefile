MAKEFLAGS=-r

out_dir := out
out_summary := summary
filename_summary := $(out_dir)/$(out_summary)

define run_summary
  echo -------------------------------------------------------------------------------- >> $(filename_summary)
  echo $(1) >> $(filename_summary)
  echo -------------------------------------------------------------------------------- >> $(filename_summary)
  stack exec sudoku-perf-exe -- sudoku17.1000.txt $(1) +RTS -s -N2 -l 2>> $(filename_summary)
  mv sudoku-perf-exe.eventlog out/$(1)-sudoku-perf-exe.eventlog
endef

.PHONY: all
all: clean directories summary

.PHONY: summary
summary:
	stack build
	$(call run_summary,v1)
	$(call run_summary,v2)
	$(call run_summary,v3)
	$(call run_summary,v4)

.PHONY: directories
directories:
	mkdir -p $(out_dir)
	touch $(filename_summary)

.PHONY: clean
clean:
	$(RM) -r $(out_dir)
