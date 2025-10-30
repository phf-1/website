###########
# Context #
###########

function service_is_active() {
    SERVICE_DIR="/etc/systemd/system"
    SERVICE_FILE="website.service"
    CACHE_DIR=/var/cache/$WEBSITE_USER/guile
    mkdir -p $CACHE_DIR
    chown $WEBSITE_USER:$WEBSITE_GROUP $CACHE_DIR
    chmod 700 $CACHE_DIR
    cp ./"$SERVICE_FILE" "$SERVICE_DIR/$SERVICE_FILE"

    # Values taken from the parent script environment.
    sed -i "s|__INSTALL_DIR__|$INSTALL_DIR|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__CACHE_DIR__|$CACHE_DIR|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_USER__|$WEBSITE_USER|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_GROUP__|$WEBSITE_GROUP|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_IP__|$WEBSITE_IP|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_PORT__|$WEBSITE_PORT|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_JS__|$WEBSITE_JS|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_CSS__|$WEBSITE_CSS|" "$SERVICE_DIR/$SERVICE_FILE"

    # Values taken from the local environment.
    source "$HOME/website_env"
    sed -i "s|__WEBSITE_LOGIN__|$WEBSITE_LOGIN|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_PASSWORD__|$WEBSITE_PASSWORD|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_CONTENT__|$WEBSITE_CONTENT|" "$SERVICE_DIR/$SERVICE_FILE"
    sed -i "s|__WEBSITE_LAYOUT__|$WEBSITE_LAYOUT|" "$SERVICE_DIR/$SERVICE_FILE"

    chown root:root "$SERVICE_DIR/$SERVICE_FILE"
    chmod 644 "$SERVICE_DIR/$SERVICE_FILE"
    systemctl daemon-reload
    systemctl enable "$SERVICE_FILE"
    systemctl stop "$SERVICE_FILE"
    systemctl start "$SERVICE_FILE"
}

#############
# Interface #
#############

function activate() {
    service_is_active
}
