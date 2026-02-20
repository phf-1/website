# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


# Makefile rules defined below use a single Bash session, strict mode and minimize
# noise written to stdout/stderr.
SHELL := bash
.SHELLFLAGS := -ceuo pipefail
MAKEFLAGS += --no-print-directory
.ONESHELL:
.SILENT:

# `make env' builds a development environment and executes an interative shell within
# a development environment.
#
# `make env CMD=...' :≡ `make env' then executes $CMD
ENV_CMD_DEFAULT="'bash --rcfile bashrc -i'"
.PHONY: env
env:
	if [[ -v GUIX_ENVIRONMENT ]]; then
	  echo "INFO | Development environment is active."
	else
	  if [[ "${CMD}" == "" ]]; then
	    CMD="${ENV_CMD_DEFAULT}"
	  else
	    CMD="${CMD}"
	  fi
	  guix shell -C -F -N -m dev-manifest.scm -- $${CMD}
	fi

# `make start' starts a webserver on port 3000
SCHEME := scheme
BIN := bin
PORT=3000
.PHONY: start
start:
	if [[ -v GUIX_ENVIRONMENT ]]; then
	  echo "INFO | The webserver is about to get started."
	  export GUILE_LOAD_PATH="${SCHEME}:${GUILE_LOAD_PATH}"
	  ${BIN}/website :start "${PORT}" library
	else
	  ${MAKE} env CMD="make start"
	fi

# `make live' starts a local development server and restarts it on file changes.
.PHONY: live
live:
	if [[ -v GUIX_ENVIRONMENT ]]; then
	  echo "INFO | About to start a webserver and live reload."
	  export GUILE_AUTO_COMPILE=0
	  # Launch server directly (so PID points to the real process)
	  export GUILE_LOAD_PATH="${SCHEME}:${GUILE_LOAD_PATH}"
	  ${BIN}/website :start "${PORT}" library &
	  PID=$$!
	  cleanup() {
	    kill $$PID 2>/dev/null || true
	    wait $$PID 2>/dev/null || true
	    echo "INFO | Live server stopped."
	    exit 0
	  }
	  trap cleanup INT TERM EXIT
	  while true; do
	    inotifywait -r -q -e close_write -e create -e delete -e move \
	      --exclude '\.git|_archive|_dist|\.swp|~$$' . >/dev/null || true
	    echo "INFO | File changes detected — restarting server..."
	    kill $$PID 2>/dev/null || true
	    wait $$PID 2>/dev/null || true
	    sleep 0.6
	    ${BIN}/website :start "${PORT}" library &
	    PID=$$!
	  done
	else
	  ${MAKE} env CMD="make live"
	fi

# `make test' executes all tests.
.PHONY: test
TEST := test
test:
	if [[ -v GUIX_ENVIRONMENT ]]; then
	  echo "INFO | Tests are about to be executed."
	  GUILE_LOAD_PATH="${SCHEME}:${TEST}:$${GUILE_LOAD_PATH}" guile ${TEST}/server.scm
	else
	  ${MAKE} env CMD="make test"
	fi

# `make archive' builds an archive containing the project files.
#   [[id:3c446152-6565-44b6-ab73-7faf6c7273b1]]
ARCHIVE_DIR := _archive
ARCHIVE := ${ARCHIVE_DIR}/archive.tar.zst
FILES := $(shell fd -t f . ./ | tr '\n' ' ')
.PHONY: archive
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
.PHONY: dist
WEBSITE := _dist/website
dist: ${WEBSITE}
${WEBSITE}: ${ARCHIVE}
	echo "INFO | Build a self-executing archive ..."
	./script/build_dist --in "$^" --out "$@"


# Given WEBSITE, then prod is a rule that executes
#
#   PORT=3000 ${WEBSITE} start
.PHONY: prod
prod: ${WEBSITE}
	echo "INFO | Given a distribution, then start it ..."
	PORT="${PORT}"
	PORT="$${PORT:-3000}" ${WEBSITE} start


# `make install USER=usr GROUP=grp EXEC=${WEBSITE} PORT=3000' implements `USER=usr
# GROUP=grp PORT=3000 ${WEBSITE} install'
.PHONY: install
ETC := etc
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

# Given the coordinates of a VPS, a user and where to send WEBSITE in the VPS, then
# deploy is a rule such that it copies WEBSITE to the VPS and execute `${WEBSITE}
# install' on the VPS.
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


# `make all' :≡ `make clean test deploy'
.PHONY: all
all: clean test deploy


# health is a rule that return /health of the deployed instance on the VPS.
.PHONY: health
health:
	curl https://phfrohring.com/health
	echo ""

# clean is a rule that deletes all generated files.
.PHONY: clean
clean:
	echo "INFO | Clean all constructed files ..."
	rm -rf _*
