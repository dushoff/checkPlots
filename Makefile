## This is checkPlots
## makestuff/project.Makefile

current: target
-include target.mk

# include makestuff/perl.def

######################################################################

# Content (all moved to "tidbits")!

Sources += $(wildcard *.R)

roswell.Rout: setY.rda roswell.R

beast.Rout: checkFuns.rda beast.R

tt.Rout: checkFuns.rda tt.R

statFuns.Rout: checkFuns.Rout statFuns.R

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp: | makestuff
	- $(RM) makestuff/*.stamp
	cd makestuff && $(MAKE) pull
	touch $@
makestuff:
	git clone --depth 1 $(msrepo)/makestuff

-include makestuff/os.mk
-include makestuff/pipeR.mk
-include makestuff/rpkg.mk

-include makestuff/git.mk

-include makestuff/visual.mk
-include makestuff/projdir.mk
