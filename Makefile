################################################################################
##
##  Makefile script as short-cut for package content management
##
##
##      Author: Jitao David Zhang <jitao_david.zhang@roche.com>
##                   
##                   - make install  calls R CMD INSTALL
##                   - make check    calls R CMD check (with RUnit)
##                   - make dist     calls R CMD build
##
################################################################################

R=R

PKG := $(shell awk 'BEGIN{FS=":"}{if ($$1=="Package") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)
PKG_VERSION=$(shell awk 'BEGIN{FS=":"}{if ($$1=="Version") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)
PKG_ROOT_DIR=`pwd`
PKG_SRC_DIR=$(PKG_ROOT_DIR)/src
KG_HIDDEN_FILES=Makefile 

install: 
	@echo '====== Installing Package ======'
	@(cd ..; ${R} CMD INSTALL $(PKG))
	@echo '====== Installing finished ======'
	@echo ' '

check: 
	@echo '====== Checking Package ======'
	@(R -e "devtools::check()")
	@echo '====== Checking finished ======'
	@echo ' '

dist:	
	@echo '====== Building Distribution ======'
	cp -rp $(PKG_ROOT_DIR) $(PKG_DIST_ROOT_DIR)
	@(cd ..; $(RM) -r $(PKG_HIDDEN_FILES); R CMD build $(PKG))
	$(RM) -r $(PKG_DIST_ROOT_DIR)
	@echo '====== Building finished ======'
	@echo ' '
	@echo '====== Checking Package ======'
	@(export R_DEVELOP_MODE=TRUE; cd ..; ${R} CMD check $(PKG)_$(PKG_VERSION).tar.gz)
	@echo '====== Checking finished  ======'
	@echo ' '

clean:
	@echo '====== Cleaning Package ======'
	@(R -e "devtools::clean_all()")
	@echo '====== Cleaning finished ======'

test:
	@echo '====== Testing Package ======'
	@(R -e "devtools::test()")
	@echo '====== Testing finished ======'

