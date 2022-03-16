#
#  Main authors:
#    Roberto Castaneda Lozano <rcas@acm.org>
#
#  This file is part of Unison, see http://unison-code.github.io
#
#  Copyright (c) 2016, RISE SICS AB
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  3. Neither the name of the copyright holder nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#

SECSOLVERCOMMONDIR := $(SECSOLVERDIR)/common
SECSOLVERMODELSDIR := $(SECSOLVERDIR)/models
SECSOLVERBRANCHERSDIR := $(SECSOLVERDIR)/branchers
SECSOLVERPROCEDURESDIR := $(SECSOLVERDIR)/procedures
SECSOLVERINSPECTORSDIR := $(SECSOLVERDIR)/inspectors

SECSOLVERMAIN = $(SECSOLVERDIR)/secsolver

SECSOLVERCOMMON := definitions util jsonutil
SECSOLVERMODELS := parameters options model localmodel completemodel globalmodel	\
simplemodel relaxedmodel secmodel
SECSOLVERBRANCHERS := filters merit value printers pressureschedulingbrancher	\
routingbrancher
SECSOLVERPROCEDURES := commonprocedures secprocedures localprocedures
SECSOLVERINSPECTORS := consoleinspector modelgraphicsview modelinspector dot	\
registerarrayinspector issuecycleinspector liverangeinspector			\
assignmentinspector allocationinspector livedurationinspector			\
selectioninspector operandassignmentinspector resourceconsumptioninspector	\
dataflowinspector alignmentinspector alignmentpartitioninspector		\
operandallocationinspector congruenceallocationinspector precedenceinspector	\
precedencematrixinspector usersinspector operandlatencyinspector

SECSOLVERCLASSES := $(addprefix $(SECSOLVERCOMMONDIR)/, $(SECSOLVERCOMMON)) $(addprefix	\
$(SECSOLVERMODELSDIR)/, $(SECSOLVERMODELS)) $(addprefix $(SECSOLVERBRANCHERSDIR)/,	\
$(SECSOLVERBRANCHERS)) $(addprefix $(SECSOLVERPROCEDURESDIR)/, $(SECSOLVERPROCEDURES))	\
$(addprefix $(SECSOLVERINSPECTORSDIR)/, $(SECSOLVERINSPECTORS))

SECSOLVERCPPSRC := $(addsuffix .cpp, $(SECSOLVERMAIN) $(SECSOLVERCLASSES))
SECSOLVERHPPSRC := $(addsuffix .hpp, $(SECSOLVERCLASSES))

SECSOLVERSRC := $(SECSOLVERCPPSRC) $(SECSOLVERHPPSRC)

UNISON_SECSOLVER_CONFIG ?= graphics

SECSOLVERPROJECT := $(SECSOLVERDIR)/secsolver.pro

GENMAKEFILE = $(SECSOLVERDIR)/sec-generated.mk

OSX_SECSOLVERBIN = $(SECSOLVERDIR)/sec-gecode-solver.app/Contents/MacOS/sec-gecode-solver

# FIXME: the presolver dependency is added to prevent linking shared object
# files that are not yet ready. The right solution is to structure the Qt
# project files appropriately, possibly using TEMPLATE and SUBDIRS.
$(SECSOLVERBIN): $(GENMAKEFILE)  $(PRESECSOLVERBIN)
	$(MAKE) -C $(SECSOLVERDIR) -f $(notdir $<)
	if [ -e $(OSX_SECSOLVERBIN) ]; then \
	    cp $(OSX_SECSOLVERBIN) $(SECSOLVERBIN); \
	fi; \

$(GENMAKEFILE): $(SECSOLVERPROJECT) $(SECSOLVERSRC)
	qmake TARGET="gecode-secsolver" CONFIG+="$(UNISON_SECSOLVER_CONFIG)" -o $@ $<

clean-secsolver:
	rm -f $(SECSOLVERDIR)/*.o $(SECSOLVERDIR)/*~ $(GENMAKEFILE) $(SECSOLVERDIR)/moc_*.cpp

veryclean-secsolver: clean-secsolver
	rm -f $(SECSOLVERBIN)
