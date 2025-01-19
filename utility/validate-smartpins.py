#!/usr/bin/env python3

import yaml


from nerdgruppe.p2.specs.smartpins import Document as SmartpinSpec

smartpins_data: dict
with open("specs/smartpins.yml") as fp:
    smartpins_data = yaml.safe_load(fp)


smartpins_spec: SmartpinSpec = SmartpinSpec.from_dict(smartpins_data)

for key, mode in smartpins_spec.smartmodes.items():
    print(key)
    print("  ", mode)
