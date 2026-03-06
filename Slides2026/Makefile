# Generate presentation PDFs from handout sources.
# Usage:
#   make present N=1      # compile Lecture 1 presentation
#   make handout N=1      # compile Lecture 1 handout
#   make all-present      # compile all presentations
#   make all-handout      # compile all handouts

# Map lecture numbers to directories
DIR_1  = w1Day1_Review
DIR_2  = w1Day2_CEFBLP
DIR_3  = w2Day3_MultivariateRegression
DIR_4  = w2Day4_Sensitivity
DIR_5  = w3Day5_EfficiencyGLS
DIR_6  = w3Day6_Heteroskedasticity
DIR_7  = w4Day7_SmallSample
DIR_9  = w5Day9_Probit
DIR_10 = w5Day10_Asymptotics
DIR_11 = w6Day11_AsymptoticsII
DIR_12 = w6Day12_HypothesisTesting
DIR_13 = w7Day13_IV
DIR_14 = w7Day14_2SLS
DIR_15 = w8Day15_GMM1
DIR_16 = w8Day16_GMM2
DIR_17 = w9Day17_Panel
DIR_18 = w9Day18_FixedEffects

ALL_LECTURES = 1 2 3 4 5 6 7 9 10 11 12 13 14 15 16 17 18

HANDOUT_SRC = $(DIR_$(N))/ILMLecture$(N)_handout.tex
PRESENT_SRC = $(DIR_$(N))/ILMLecture$(N)_presentation.tex

# Build a presentation PDF by stripping "handout, " from the handout source
present:
ifndef N
	$(error Usage: make present N=<lecture number>)
endif
	@echo "Building presentation for Lecture $(N)..."
	sed 's/handout, //' $(HANDOUT_SRC) > $(PRESENT_SRC)
	cd $(DIR_$(N)) && pdflatex ILMLecture$(N)_presentation.tex && pdflatex ILMLecture$(N)_presentation.tex
	@echo "Done: $(DIR_$(N))/ILMLecture$(N)_presentation.pdf"

# Build a handout PDF
handout:
ifndef N
	$(error Usage: make handout N=<lecture number>)
endif
	@echo "Building handout for Lecture $(N)..."
	cd $(DIR_$(N)) && pdflatex ILMLecture$(N)_handout.tex && pdflatex ILMLecture$(N)_handout.tex
	@echo "Done: $(DIR_$(N))/ILMLecture$(N)_handout.pdf"

# Build all presentations
all-present:
	@for n in $(ALL_LECTURES); do \
		$(MAKE) present N=$$n || true; \
	done

# Build all handouts
all-handout:
	@for n in $(ALL_LECTURES); do \
		$(MAKE) handout N=$$n || true; \
	done

# Clean generated presentation files
clean-present:
	rm -f */ILMLecture*_presentation.*

.PHONY: present handout all-present all-handout clean-present
