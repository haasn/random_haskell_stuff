#include <stdlib.h>
#include "/usr/include/weechat/weechat-plugin.h"

#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "WeeHaskell_stub.h"
#endif


WEECHAT_PLUGIN_NAME("WeeHaskell");
WEECHAT_PLUGIN_DESCRIPTION("Haskell plugin library");
WEECHAT_PLUGIN_AUTHOR("nand <nand@lavabit.com>");
WEECHAT_PLUGIN_VERSION("0.1");
WEECHAT_PLUGIN_LICENSE("GPL3");

// Init and End code needs to create/destroy the haskell runtime

int weechat_plugin_init (struct t_weechat_plugin *plugin, int argc, char *argv[])
{
	hs_init(&argc, &argv);

/*
#ifdef __GLASGOW_HASKELL
	hs_add_root(__stginit_Semantic);
#endif
*/

	return WEECHAT_RC_OK;
}

int weechat_plugin_end (struct t_weechat_plugin *plugin)
{
	hs_exit();

	return WEECHAT_RC_OK;
}

