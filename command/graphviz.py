#!/usr/bin/env python3

import subprocess
import sys
import xml.etree.ElementTree as Tree

WIDTH = 1920
HEIGHT = 1080

SVG_NS = "http://www.w3.org/2000/svg"
XLINK_NS = "http://www.w3.org/1999/xlink"


def main() -> None:
    input_path = sys.argv[1]
    output_path = input_path + ".svg"

    svg = subprocess.run(
        ["dot", "-Tsvg", input_path], check=True, capture_output=True
    ).stdout

    Tree.register_namespace("", SVG_NS)
    Tree.register_namespace("xlink", XLINK_NS)

    root = Tree.fromstring(svg)
    root.set("width", str(WIDTH))
    root.set("height", str(HEIGHT))
    root.set("preserveAspectRatio", "xMidYMid meet")
    root.set("style", "background:#fff")

    Tree.ElementTree(root).write(
        output_path, xml_declaration=True, encoding="UTF-8")


if __name__ == "__main__":
    main()
