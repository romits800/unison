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

SECDIVERSIFIERCOMMONDIR := $(SECDIVERSIFIERDIR)/common
SECDIVERSIFIERMODELSDIR := $(SECDIVERSIFIERDIR)/models
SECDIVERSIFIERBRANCHERSDIR := $(SECDIVERSIFIERDIR)/branchers
SECDIVERSIFIERPROCEDURESDIR := $(SECDIVERSIFIERDIR)/procedures
SECDIVERSIFIERINSPECTORSDIR := $(SECDIVERSIFIERDIR)/inspectors

SECDIVERSIFIERMAIN = $(SECDIVERSIFIERDIR)/secdiversifier

SECDIVERSIFIERCOMMON := definitions util jsonutil
SECDIVERSIFIERMODELS := parameters solver-parameters options model localmodel completemodel globalmodel	\
simplemodel relaxedmodel secmodel seclocaldivmodel secdivmodel secdecompdivmodel
SECDIVERSIFIERBRANCHERS := filters merit value printers pressureschedulingbrancher	\
routingbrancher
SECDIVERSIFIERPROCEDURES := commonprocedures secdivprocedures localdivprocedures
SECDIVERSIFIERINSPECTORS := consoleinspector modelgraphicsview modelinspector dot	\
registerarrayinspector issuecycleinspector liverangeinspector			\
assignmentinspector allocationinspector livedurationinspector			\
selectioninspector operandassignmentinspector resourceconsumptioninspector	\
dataflowinspector alignmentinspector alignmentpartitioninspector		\
operandallocationinspector congruenceallocationinspector precedenceinspector	\
precedencematrixinspector usersinspector operandlatencyinspector

SECDIVERSIFIERCLASSES := $(addprefix $(SECDIVERSIFIERCOMMONDIR)/, $(SECDIVERSIFIERCOMMON)) $(addprefix	\
$(SECDIVERSIFIERMODELSDIR)/, $(SECDIVERSIFIERMODELS)) $(addprefix $(SECDIVERSIFIERBRANCHERSDIR)/,	\
$(SECDIVERSIFIERBRANCHERS)) $(addprefix $(SECDIVERSIFIERPROCEDURESDIR)/, $(SECDIVERSIFIERPROCEDURES))	\
$(addprefix $(SECDIVERSIFIERINSPECTORSDIR)/, $(SECDIVERSIFIERINSPECTORS))

SECDIVERSIFIERCPPSRC := $(addsuffix .cpp, $(SECDIVERSIFIERMAIN) $(SECDIVERSIFIERCLASSES))
SECDIVERSIFIERHPPSRC := $(addsuffix .hpp, $(SECDIVERSIFIERCLASSES))

SECDIVERSIFIERSRC := $(SECDIVERSIFIERCPPSRC) $(SECDIVERSIFIERHPPSRC)

UNISON_SECDIVERSIFIER_CONFIG ?= graphics

SECDIVERSIFIERPROJECT := $(SECDIVERSIFIERDIR)/secdiversifier.pro

GENMAKEFILE = $(SECDIVERSIFIERDIR)/secdiv-generated.mk

OSX_SECDIVERSIFIERBIN = $(SECDIVERSIFIERDIR)/gecode-secdiversify.app/Contents/MacOS/gecode-secdiversify

# FIXME: the presolver dependency is added to prevent linking shared object
# files that are not yet ready. The right solution is to structure the Qt
# project files appropriately, possibly using TEMPLATE and SUBDIRS.
$(SECDIVERSIFIERBIN): $(GENMAKEFILE)  $(PRESOLVERBIN)
	$(MAKE) -C $(SECDIVERSIFIERDIR) -f $(notdir $<)
	if [ -e $(OSX_SECDIVERSIFIERBIN) ]; then \
	    cp $(OSX_SECDIVERSIFIERBIN) $(SECDIVERSIFIERBIN); \
	fi; \

$(GENMAKEFILE): $(SECDIVERSIFIERPROJECT) $(SECDIVERSIFIERSRC)
	qmake TARGET="gecode-secdiversifier" CONFIG+="$(UNISON_SECDIVERSIFIER_CONFIG)" -o $@ $<

clean-secdiversifier:
	rm -f $(SECDIVERSIFIERDIR)/*.o $(SECDIVERSIFIERDIR)/*~ $(GENMAKEFILE) $(SECDIVERSIFIERDIR)/moc_*.cpp

veryclean-secdiversifier: clean-secdiversifier
	rm -f $(SECDIVERSIFIERBIN)
