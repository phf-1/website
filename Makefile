# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


#################
# Specification #
#################
#
# Makefile rules defined below use a single Bash session, strict mode and minimize
# noise written to stdout/stderr.

##################
# Implementation #
##################

SHELL := bash
.SHELLFLAGS := -ceuo pipefail
MAKEFLAGS += --no-print-directory
.ONESHELL:
.SILENT:


#################
# Specification #
#################
#
# env is a rule that builds a development environment that contains all necessary
# dependencies to execute the following rules.

##################
# Implementation #
##################

.PHONY: env
env:
	echo "INFO | Build a development environment."
	guix shell -C -F -N -m dev-manifest.scm


#################
# Specification #
#################
#
# Given that the current environment is built by env, and port, then start is a rule
# that starts the web server.

##################
# Implementation #
##################

.PHONY: start
SCHEME := ${PWD}/scheme
BIN := ${PWD}/bin
start:
	echo "INFO | Given a development environment is active, then build a web server ..."
	PORT="$${PORT:-3000}"
	export GUILE_LOAD_PATH="${SCHEME}:${GUILE_LOAD_PATH}"
	${BIN}/website :start "$${PORT}" ${PWD}/library


#################
# Specification #
#################
#
# test is a rule that executes all tests.

##################
# Implementation #
##################

.PHONY: test
TEST := ${PWD}/test
test:
	echo "INFO | Execute tests ..."
	guix shell -C -F -N -m dev-manifest.scm -- bash -c 'GUILE_LOAD_PATH="${SCHEME}:${TEST}:$${GUILE_LOAD_PATH}" guile ${TEST}/server.scm'


#################
# Specification #
#################
#
#+ID: 3c446152-6565-44b6-ab73-7faf6c7273b1
# archive is a rule that builds an archive containing the project files.

##################
# Implementation #
##################

.PHONY: archive
ARCHIVE_DIR := ${PWD}/_archive
ARCHIVE := ${ARCHIVE_DIR}/archive.tar.zst
FILES := $(shell fd -t f . ./ | tr '\n' ' ')
archive: ${ARCHIVE}
${ARCHIVE}: ${FILES}
	echo "INFO | Build an archive ..."
	TMPDIR="${ARCHIVE_DIR}/archive"
	mkdir -p "$${TMPDIR}"
	cp -rf Makefile "$${TMPDIR}"
	cp -rf bibliography.bib "$${TMPDIR}"
	cp -rf bin "$${TMPDIR}"
	cp -rf csl "$${TMPDIR}"
	cp -rf css "$${TMPDIR}"
	cp -rf elisp "$${TMPDIR}"
	cp -rf etc "$${TMPDIR}"
	cp -rf html "$${TMPDIR}"
	cp -rf js "$${TMPDIR}"
	cp -rf library "$${TMPDIR}"
	cp -rf prod-manifest.scm "$${TMPDIR}"
	cp -rf scheme "$${TMPDIR}"
	cp -rf script "$${TMPDIR}"
	rm -rf "$@"
	tar -C ${ARCHIVE_DIR} -cf - archive | zstd -19 -o "$@"
	rm -rf "$${TMPDIR}"


#################
# Specification #
#################
#
# Given ARCHIVE, then dist is a rule that builds a single executable WEBSITE
# such that after:
#
#   PORT=3000 ${WEBSITE} start
#
#     then a webserver process that satisfies HTTP protocol serves requests on
#     localhost:3000. The publications served are the one contained in ARCHIVE.
#
#   USER=usr GROUP=grp PORT=3000 ${WEBSITE} install
#
#     then a sysD service is installed on the current system that supervises a
#     process started with `PORT=3000 ${WEBSITE} start' by the user usr.

##################
# Implementation #
##################

.PHONY: dist
WEBSITE := ${PWD}/_dist/website
dist: ${WEBSITE}
${WEBSITE}: ${ARCHIVE}
	echo "INFO | Build a self-executing archive ..."
	./script/build_dist --in "$^" --out "$@"


#################
# Specification #
#################
#
# Given WEBSITE, then prod is a rule that executes
#
#   PORT=3000 ${WEBSITE} start

##################
# Implementation #
##################

.PHONY: prod
prod: ${WEBSITE}
	echo "INFO | Given a distribution, then start it ..."
	PORT="${PORT}"
	PORT="$${PORT:-3000}" ${WEBSITE} start


#################
# Specification #
#################
#
# `make install USER=usr GROUP=grp EXEC=${WEBSITE} PORT=3000' implements `USER=usr
# GROUP=grp PORT=3000 ${WEBSITE} install'

##################
# Implementation #
##################

.PHONY: install
ETC := ${PWD}/etc
install: ${WEBSITE}
	echo "INFO | Given a distribution and root priviledges, then install and activate a sysD service ..."
	[[ $$EUID -eq 0 ]] || { echo "ERROR | Current user is not root."; exit 1; }
	chown "${USER}:${GROUP}" "${EXEC}"
	SERVICE=/tmp/website.service.tmp
	sed 's|@USER@|${USER}|g' ${ETC}/systemd/system/website.service > "$$SERVICE"
	sed -i 's|@EXEC@|${EXEC}|g' "$$SERVICE"
	sed -i 's|@PORT@|${PORT}|g' "$$SERVICE"
	install -m 644 "$$SERVICE" /etc/systemd/system/website.service
	rm "$$SERVICE"
	systemctl daemon-reload
	systemctl stop website.service
	systemctl enable website.service
	systemctl start website.service
	systemctl status website.service


#################
# Specification #
#################
#
# Given the coordinates of a VPS, a user and where to send WEBSITE in the VPS, then
# deploy is a rule such that it copies WEBSITE to the VPS and execute `${WEBSITE}
# install' on the VPS.

##################
# Implementation #
##################

.PHONY: deploy
VPS_IP=138.197.186.104
VPS_PORT=3000
VPS_USER=websiteuser
VPS_GROUP=websitegroup
VPS_EXEC=/opt/website/bin/website
deploy: ${WEBSITE}
	echo "INFO | Given a distribution and appropriate ssh key, then install and activate a sysD service on VPS ..."
	rsync -avz "$^" root@${VPS_IP}:${VPS_EXEC}
	set -x
	ssh root@${VPS_IP} 'USER=${VPS_USER} GROUP=${VPS_GROUP} PORT=${VPS_PORT} ${VPS_EXEC} install'
	set +x


#################
# Specification #
#################
#
# `make all' :≡ `make clean test deploy'

##################
# Implementation #
##################

.PHONY: all
all: clean test deploy


#################
# Specification #
#################
#
# health is a rule that return /health of the deployed instance on the VPS.

##################
# Implementation #
##################

.PHONY: health
health:
	curl https://phfrohring.com/health
	echo ""


#################
# Specification #
#################
#
# clean is a rule that deletes all generated files.

##################
# Implementation #
##################

.PHONY: clean
clean:
	echo "INFO | Clean all constructed files ..."
	rm -rf _*
