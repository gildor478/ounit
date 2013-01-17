oasis = require("oasis")
darcs = require("darcs")
ci = require("ci")

ci.init()
oasis.init()
darcs.init()

oasis.std_process()
darcs.create_tag(oasis.package_version())
