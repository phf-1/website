###########
# Context #
###########

CONF="$HOME/website_env"
if [[ ! -f "$CONF" ]]; then
    echo "ERROR | CONF is not a file. CONF = $CONF"
    exit 1
fi

source "$CONF"

executing_as_root() {
    if [ "$(id -u)" -ne 0 ]; then
        echo "This script must be run as root (use sudo)" >&2
        exit 1
    fi
}

system_is_updated() {
    apt update
    apt upgrade -y
}

locales_are_configured() {
    apt install -y locales
    sed -i 's|# C.UTF-8 UTF-8|C.UTF-8 UTF-8|' /etc/locale.gen
    locale-gen
    BASHRC=~/.bashrc
    LANG_LOCALE='export LANG=C.UTF-8'
    if ! grep -Fx "$LANG_LOCALE" "$BASHRC" &>/dev/null; then
        echo "$LANG_LOCALE" >> "$BASHRC"
    fi
    LC_LOCALE='export LC_ALL=C.UTF-8'
    if ! grep -Fx "$LC_LOCALE" "$BASHRC" &>/dev/null; then
        echo "$LC_LOCALE" >> "$BASHRC"
    fi
    LANGUAGE_LOCALE='export LANGUAGE=en'
    if ! grep -Fx "$LANGUAGE_LOCALE" "$BASHRC" &>/dev/null; then
        echo "$LANGUAGE_LOCALE" >> "$BASHRC"
    fi
    source "$BASHRC"
}

dependencies_are_installed() {
    apt update
    apt install -y \
        guile-3.0 \
        guile-gcrypt \
        guile-json \

    apt install logrotate
    systemctl enable logrotate.timer
    systemctl start logrotate.timer
}

user_and_group_exist() {
    if ! getent group "$WEBSITE_GROUP" >/dev/null; then
        groupadd -r "$WEBSITE_GROUP"
    fi
    if ! getent passwd "$WEBSITE_USER" >/dev/null; then
        useradd -r -g "$WEBSITE_GROUP" -d /nonexistent -s /usr/sbin/nologin "$WEBSITE_USER"
    fi
}

install_dir_has_been_built() {
    mkdir -p "$INSTALL_DIR"/{bin,scheme,layout,js,css}
    cp -r ./bin/* "$INSTALL_DIR/bin/"
    cp -r ./scheme/* "$INSTALL_DIR/scheme/"
    cp -r ./layout/* "$INSTALL_DIR/layout/"
    cp -r ./js/* "$INSTALL_DIR/js/"
    cp -r ./css/* "$INSTALL_DIR/css/"
    find "$INSTALL_DIR" -type d -exec chmod 755 {} \;
    find "$INSTALL_DIR" -type f -exec chmod 644 {} \;
    chmod 755 "$INSTALL_DIR/bin/website"
    chown -R "$WEBSITE_USER:$WEBSITE_GROUP" "$INSTALL_DIR"
    ln -sf "$INSTALL_DIR/bin/website" "/usr/local/bin/website"
}

proxy_service_is_active() {
    apt install nginx -y
    apt install certbot python3-certbot-nginx -y
    # certbot --nginx -d phfrohring.com -d www.phfrohring.com
    NGINX_CONF="config/nginx.conf"
    cp -vf $NGINX_CONF /etc/nginx/nginx.conf
    SERVER_CONF="config/phfrohring.conf"
    cp -vf $SERVER_CONF /etc/nginx/sites-available/default
    sed -i "s|__IP__|$WEBSITE_IP|" /etc/nginx/sites-available/default
    sed -i "s|__PORT__|$WEBSITE_PORT|" /etc/nginx/sites-available/default
    systemctl start nginx
    systemctl enable nginx
    systemctl reload nginx
}

firewall_is_active() {
    apt install ufw -y
    ufw allow 'Nginx Full'
    ufw allow OpenSSH
    ufw --force enable
    ufw reload
}

#############
# Interface #
#############

install() {
    executing_as_root
    system_is_updated
    locales_are_configured
    dependencies_are_installed
    user_and_group_exist
    install_dir_has_been_built
    proxy_service_is_active
    firewall_is_active
}
