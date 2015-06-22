PROJECT = toolbox

DEPS = lager
#DEPS_DIR = ..

ERLC_OPTS := -Werror +debug_info +warn_export_all +warn_export_vars  +warn_shadow_vars +warn_obsolete_guard +'{lager_truncation_size, 8192}' +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS := +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +'{parse_transform, eunit_autoexport}' +'{parse_transform, lager_transform}'

include erlang.mk

run: all
	erl -pa ebin -s toolbox -boot start_sasl -name ${NAME} -setcookie ${COOKIE}
