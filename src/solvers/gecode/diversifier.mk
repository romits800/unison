#
#  Main authors:
#    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
#    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
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

DIVERSIFIERCOMMONDIR := $(DIVERSIFIERDIR)/common
DIVERSIFIERMODELSDIR := $(DIVERSIFIERDIR)/models
DIVERSIFIERBRANCHERSDIR := $(DIVERSIFIERDIR)/branchers
DIVERSIFIERPROCEDURESDIR := $(DIVERSIFIERDIR)/procedures
DIVERSIFIERINSPECTORSDIR := $(DIVERSIFIERDIR)/inspectors

DIVERSIFIERMAIN = $(DIVERSIFIERDIR)/diversifier

DIVERSIFIERCOMMON := definitions util jsonutil
DIVERSIFIERMODELS := parameters solver-parameters options model localmodel completemodel globalmodel	\
simplemodel relaxedmodel localdivmodel divmodel decompdivmodel
DIVERSIFIERBRANCHERS := filters merit value printers pressureschedulingbrancher	\
routingbrancher
DIVERSIFIERPROCEDURES := commonprocedures divprocedures localdivprocedures
DIVERSIFIERINSPECTORS := consoleinspector modelgraphicsview modelinspector dot	\
registerarrayinspector issuecycleinspector liverangeinspector			\
assignmentinspector allocationinspector livedurationinspector			\
selectioninspector operandassignmentinspector resourceconsumptioninspector	\
dataflowinspector alignmentinspector alignmentpartitioninspector		\
operandallocationinspector congruenceallocationinspector precedenceinspector	\
precedencematrixinspector usersinspector operandlatencyinspector

DIVERSIFIERCLASSES := $(addprefix $(DIVERSIFIERCOMMONDIR)/, $(DIVERSIFIERCOMMON)) $(addprefix	\
$(DIVERSIFIERMODELSDIR)/, $(DIVERSIFIERMODELS)) $(addprefix $(DIVERSIFIERBRANCHERSDIR)/,	\
$(DIVERSIFIERBRANCHERS)) $(addprefix $(DIVERSIFIERPROCEDURESDIR)/, $(DIVERSIFIERPROCEDURES))	\
$(addprefix $(DIVERSIFIERINSPECTORSDIR)/, $(DIVERSIFIERINSPECTORS))

DIVERSIFIERCPPSRC := $(addsuffix .cpp, $(DIVERSIFIERMAIN) $(DIVERSIFIERCLASSES))
DIVERSIFIERHPPSRC := $(addsuffix .hpp, $(DIVERSIFIERCLASSES))

DIVERSIFIERSRC := $(DIVERSIFIERCPPSRC) $(DIVERSIFIERHPPSRC)

UNISON_DIVERSIFIER_CONFIG ?= graphics

DIVERSIFIERPROJECT := $(DIVERSIFIERDIR)/diversifier.pro

GENMAKEFILE = $(DIVERSIFIERDIR)/div-generated.mk

OSX_DIVERSIFIERBIN = $(DIVERSIFIERDIR)/gecode-diversify.app/Contents/MacOS/gecode-diversify

# FIXME: the presolver dependency is added to prevent linking shared object
# files that are not yet ready. The right solution is to structure the Qt
# project files appropriately, possibly using TEMPLATE and SUBDIRS.
$(DIVERSIFIERBIN): $(GENMAKEFILE)  $(PRESOLVERBIN)
	$(MAKE) -C $(DIVERSIFIERDIR) -f $(notdir $<)
	if [ -e $(OSX_DIVERSIFIERBIN) ]; then \
	    cp $(OSX_DIVERSIFIERBIN) $(DIVERSIFIERBIN); \
	fi; \

$(GENMAKEFILE): $(DIVERSIFIERPROJECT) $(DIVERSIFIERSRC)
	qmake TARGET="gecode-diversifier" CONFIG+="$(UNISON_DIVERSIFIER_CONFIG)" -o $@ $<

clean-diversifier:
	rm -f $(DIVERSIFIERDIR)/*.o $(DIVERSIFIERDIR)/*~ $(GENMAKEFILE) $(DIVERSIFIERDIR)/moc_*.cpp

veryclean-diversifier: clean-diversifier
	rm -f $(DIVERSIFIERBIN)
