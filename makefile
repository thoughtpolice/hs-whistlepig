VERSION=0.12
NAME=whistlepig-$(VERSION)
TAR=$(NAME).tar.gz
URL=http://masanjin.net/whistlepig/$(TAR)
OUT=$(shell pwd)/src/cbits/whistlepig

Q?=@

update:
# Extract tarball
	$(Q)echo -n 'Downloading $(NAME)... '
	$(Q)wget --quiet $(URL)
	$(Q)tar zxf $(TAR)
	$(Q)echo OK

# Append to whistlepig makefile to make it copy stuff
# for us
	$(Q)echo -n 'Patching build... '
	$(Q)echo 'cabal: $$(EXPORTFILES)' >> $(NAME)/Makefile
	$(Q)echo '	cp README $(OUT)' >> $(NAME)/Makefile
	$(Q)echo '	cp README $(OUT)' >> $(NAME)/Makefile
	$(Q)echo '	cp $$+ $(OUT)' >> $(NAME)/Makefile
	$(Q)echo OK

# Out with the old, in with the new
	$(Q)echo -n 'Copying source code into $(OUT)...'
	$(Q)rm -rf $(OUT)
	$(Q)mkdir $(OUT)
	$(Q)make --quiet -C $(NAME) cabal 2>&1 /dev/null # copies to $(OUT)
	$(Q)rm -rf $(TAR) $(NAME)
	$(Q)echo OK

	$(Q)echo Done!

clean:
	rm -rf *~ dist*
uberclean: clean
	rm -rf $(OUT)
	mkdir $(OUT)
