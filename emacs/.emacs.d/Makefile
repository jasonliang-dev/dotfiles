MK_FILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MK_FILE_DIR := $(dir $(MK_FILE_PATH)) # aka emacs directory

# Install all packages in config.  It's nice to do this in the cli
# since I restart emacs after a fresh install anyways.  C-u doesn't
# scroll the view as intended, fonts don't load correctly, etc.  These
# weird bugs disappear the next time emacs is launched.
all:
	emacs --batch -l $(MK_FILE_DIR)init.el

# Remove some byte compiled files
clean:
	rm -f $(MK_FILE_DIR)custom.elc
	rm -f $(MK_FILE_DIR)init.elc
	rm -f $(MK_FILE_DIR)lang/*.elc
	rm -f $(MK_FILE_DIR)lisp/*.elc
