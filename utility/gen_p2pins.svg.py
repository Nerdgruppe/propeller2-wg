#!/usr/bin/env python

from abc import ABC, abstractmethod
from lxml.etree import Element, tostring as render_xml, SubElement, _Element
from dataclasses import dataclass, field, replace
from typing import Literal


@dataclass(frozen=True, kw_only=False, eq=True)
class Condition:
    mask: int
    pattern: int


@dataclass(frozen=True, kw_only=False, eq=True)
class Point:
    x: int
    y: int

    def __str__(self) -> str:
        return f"({self.x},{self.y})"

    def offset(self, dx: int, dy: int) -> "Point":
        return Point(self.x + dx, self.y + dy)


# from https://stackoverflow.com/a/15232899
def project_point_to_line(line1: Point, line2: Point, pt: Point) -> Point | None:
    if line1.x == line2.x and line1.y == line2.y:
        line1.x -= 0.00001

    U = ((pt.x - line1.x) * (line2.x - line1.x)) + ((pt.y - line1.y) * (line2.y - line1.y))
    Udenom = pow(line2.x - line1.x, 2) + pow(line2.y - line1.y, 2)
    U /= Udenom

    r = Point(
        x=line1.x + (U * (line2.x - line1.x)),
        y=line1.y + (U * (line2.y - line1.y)),
    )

    minx = min(line1.x, line2.x)
    maxx = max(line1.x, line2.x)

    miny = min(line1.y, line2.y)
    maxy = max(line1.y, line2.y)

    # valid = (r.x >= minx and r.x <= maxx) and (r.y >= miny and r.y <= maxy)
    # if not valid:
    # return None
    return r


@dataclass(kw_only=True)
class Graphic(ABC):
    enable: Condition | None = field(default=None)

    @abstractmethod
    def render(self, root: _Element) -> None: ...

    def set_attribs(self, elem: _Element) -> None:
        "Sets common graphic attributes on 'elem'."

        if self.enable is not None:
            elem.set("data-mask", f"{self.enable.mask:08X}")
            elem.set("data-value", f"{self.enable.value:08X}")


@dataclass(kw_only=True)
class Box(Graphic):
    x: int
    y: int
    width: int
    height: int

    color: str = field(default="#CCCCCC")
    border: str | None = field(default=None)
    label: str = field(default="")

    fontcolor: str = field(default="#FFFFFF")

    @property
    def right(self) -> int:
        return self.x + self.width

    @property
    def bottom(self) -> int:
        return self.y + self.height

    def anchor(self, edge: Literal["t"] | Literal["b"] | Literal["l"] | Literal["r"], pos: float = 0.5) -> Point:
        def scale(v: int) -> int:
            return int(v * pos + 0.5)

        match edge:
            case "t":
                return Point(self.x + scale(self.width), self.y)

            case "b":
                return Point(self.x + scale(self.width), self.y + self.height)

            case "l":
                return Point(self.x, self.y + scale(self.height))

            case "r":
                return Point(self.x + self.width, self.y + scale(self.height))

            case _:
                raise ValueError("Invalid edge")

    def edge(self, name: Literal["t"] | Literal["b"] | Literal["l"] | Literal["r"]) -> "Line":
        match name:
            case "t":
                return Line(
                    x1=self.x,
                    y1=self.y,
                    x2=self.x + self.width,
                    y2=self.y,
                )

            case "b":
                return Line(
                    x1=self.x,
                    y1=self.y + self.height,
                    x2=self.x + self.width,
                    y2=self.y + self.height,
                )

            case "l":
                return Line(
                    x1=self.x,
                    y1=self.y,
                    x2=self.x,
                    y2=self.y + self.height,
                )

            case "r":
                return Line(
                    x1=self.x + self.width,
                    y1=self.y,
                    x2=self.x + self.width,
                    y2=self.y + self.height,
                )

            case _:
                raise ValueError("Invalid edge")

    def render(self, root: _Element) -> None:
        rect = SubElement(
            root,
            "rect",
            x=str(self.x),
            y=str(self.y),
            width=str(self.width),
            height=str(self.height),
            fill=self.color,
        )
        self.set_attribs(rect)
        if self.border is not None:
            rect.set("stroke", self.border)
        if self.label.strip() != "":
            text = SubElement(
                root,
                "text",
                x=str(self.x + self.width / 2),
                y=str(self.y + self.height / 2),
                fill=self.fontcolor,
            )

            text.set("alignment-baseline", "middle")
            text.set("text-anchor", "middle")
            text.text = self.label


@dataclass(kw_only=True)
class Line(Graphic):
    x1: int
    y1: int
    x2: int
    y2: int

    color: str = field(default="#CCCCCC")

    def render(self, root: _Element) -> _Element:
        line = SubElement(
            root,
            "line",
            x1=str(self.x1),
            y1=str(self.y1),
            x2=str(self.x2),
            y2=str(self.y2),
            stroke=self.color,
        )
        self.set_attribs(line)
        return line

    @property
    def start(self) -> Point:
        return Point(self.x1, self.y1)

    @property
    def end(self) -> Point:
        return Point(self.x2, self.y2)

    def at(self, perc: float) -> Point:
        return Point(
            int(self.x1 + perc * (self.x2 - self.x1) + 0.5),
            int(self.y1 + perc * (self.y2 - self.y1) + 0.5),
        )


@dataclass(kw_only=True)
class Arrow(Line):
    head_marker: Literal[""] | Literal["arrow"] | Literal["circle"] = field(default="arrow")
    tail_marker: Literal[""] | Literal["arrow"] | Literal["circle"] = field(default="")

    head_label: str = field(default="")
    tail_label: str = field(default="")
    label: str = field(default="")

    fontcolor: str = field(default="#888888")

    def render(self, root: _Element) -> None:
        line = super().render(root)

        if self.head_marker != "":
            line.set("marker-end", f"url(#{self.head_marker})")
        if self.tail_marker != "":
            line.set("marker-start", f"url(#{self.tail_marker})")

        dx = self.x2 - self.x1
        dy = self.y2 - self.y1

        anchors: tuple[str, str]
        if abs(dx) < 5:
            anchors = ("middle", "middle")
        elif dx < 0:
            anchors = ("end", "start")
        else:
            anchors = ("start", "end")

        baselines: tuple[str, str]
        if abs(dy) < 5:
            baselines = ("middle", "middle")
        elif dy < 0:
            baselines = ("after-edge", "before-edge")
        else:
            baselines = ("before-edge", "after-edge")

        if self.head_label.strip() != "":
            pt = self.at(1.02)
            text = SubElement(
                root,
                "text",
                x=str(pt.x),
                y=str(pt.y),
                fill=self.fontcolor,
            )

            text.set("text-anchor", anchors[0])
            text.set("alignment-baseline", baselines[0])
            text.text = self.head_label

        if self.tail_label.strip() != "":
            pt = self.at(-0.02)
            text = SubElement(
                root,
                "text",
                x=str(pt.x),
                y=str(pt.y),
                fill=self.fontcolor,
            )

            text.set("text-anchor", anchors[1])
            text.set("alignment-baseline", baselines[1])
            text.text = self.tail_label

        if self.label.strip() != "":
            pt = self.at(0.5)
            text = SubElement(
                root,
                "text",
                x=str(pt.x),
                y=str(pt.y),
                fill=self.fontcolor,
            )

            text.set("text-anchor", "middle")
            text.set("alignment-baseline", "after-edge")
            text.text = self.label


def render() -> list[Graphic]:
    elements: list[Graphic] = list()

    def coord(val: int | Point, axis: Literal["x"] | Literal["y"]) -> int:
        if isinstance(val, Point):
            return getattr(val, axis)
        return val

    def box(x, y, w, h, **kwargs) -> Box:
        b = Box(
            x=coord(x, "x"),
            y=coord(y, "y"),
            width=w,
            height=h,
            **kwargs,
        )
        elements.append(b)
        return b

    def line(x1, y1, x2, y2, **kwargs) -> Line:
        ln = Line(
            x1=coord(x1, "x"),
            y1=coord(y1, "y"),
            x2=coord(x2, "x"),
            y2=coord(y2, "y"),
            **kwargs,
        )
        elements.append(ln)
        return ln

    def arrow(start: Point | Line, end: Point | Line, **kwargs) -> Line:
        assert not (isinstance(start, Line) and isinstance(end, Line)), "Arrows must start or end at a defined point!"

        x1: int
        y1: int
        x2: int
        y2: int

        def _project_to_line(x: int, y: int, line: Line) -> tuple[int, int]:
            pass

        if isinstance(start, Point):
            x1, y1 = (start.x, start.y)

        if isinstance(end, Point):
            x2, y2 = (end.x, end.y)

        if isinstance(start, Line):
            pt = project_point_to_line(start.start, start.end, Point(x2, y2))
            x1, y1 = (pt.x, pt.y)

        if isinstance(end, Line):
            pt = project_point_to_line(end.start, end.end, Point(x1, y1))
            x2, y2 = (pt.x, pt.y)

        ln = Arrow(
            x1=x1,
            y1=y1,
            x2=x2,
            y2=y2,
            **kwargs,
        )
        elements.append(ln)
        return ln

    # Background Boxes
    box(848, 100, 775, 888, color="#dbeef4", border="#b7dee8")  # top-right
    box(848, 988, 770, 786, color="#dbeef4", border="#b7dee8")  # bottom-right
    box(111, 100, 738, 887, color="#f3f6e2", border="#e7ecc6")  # top-left
    box(111, 988, 738, 796, color="#f3f6e2", border="#e7ecc6")  # bottom-left

    # Even I/O Ring

    even_pin = box(174, 350, 150, 125, color="#984807", label="Physical Even # Pin")

    even_dac = box(458, 282, 197, 144, color="#4f81bd", label="Flash DAC Network (%M...M)")
    even_out = box(457, 468, 197, 144, color="#376092", label="Logic Drive (%M...M)")
    even_inp = box(460, 681, 197, 110, color="#31859c", label="Comparator & Logic & Schmitt (%M...M)")
    even_adc = box(462, 814, 197, 111, color="#948a54", label="Sigma-Delta ADC (%M...M)")

    even_mux = box(726, 792, 104, 83, color="#604a7b", label="MUX (%M...M)")

    # Odd I/O Ring
    odd_pin = box(174, 1195, 150, 125, color="#984807", label="Physical Odd # Pin")

    odd_dac = box(459, 1125, 197, 144, color="#4f81bd", label="Flash DAC Network (%M...M)")
    odd_out = box(458, 1311, 197, 144, color="#376092", label="Logic Drive (%M...M)")
    odd_inp = box(457, 1518, 197, 110, color="#31859c", label="Comparator & Logic & Schmitt (%M...M)")
    odd_adc = box(460, 1651, 197, 111, color="#948a54", label="Sigma-Delta ADC (%M...M)")

    odd_mux = box(724, 1630, 104, 83, color="#604a7b", label="MUX (%M...M)")

    # I/O Ring Interconnect

    even_pad = line(404, even_dac.anchor("l"), 404, odd_inp.anchor("l", 0.66), color="#984807")
    odd_pad = line(375, even_inp.anchor("l", 0.66), 375, odd_adc.anchor("l"), color="#984807")

    # even
    arrow(even_pin.anchor("r"), even_pad, color=even_pad.color, head_marker="circle")

    arrow(even_dac.anchor("l"), even_pad, color=even_dac.color)
    arrow(even_out.anchor("l"), even_pad, color=even_out.color)

    arrow(
        even_pad,
        even_inp.anchor("l", 0.33),
        color=even_pad.color,
        tail_marker="circle",
        head_label="Pin A",
        fontcolor="#ffff00",
    )
    arrow(odd_pad, even_inp.anchor("l", 0.66), color=odd_pad.color, head_label="Pin B", fontcolor="#ffff00")

    arrow(
        even_pad,
        even_adc.anchor("l"),
        color=even_pad.color,
        tail_marker="circle",
        head_label="Pin A",
        fontcolor="#ffff00",
    )

    even_sig_input = arrow(
        even_inp.anchor("r"), even_inp.anchor("r").offset(50, 0), head_marker="circle", color=even_inp.color
    )
    arrow(even_sig_input.end, even_out.anchor("b"), color=even_inp.color)
    arrow(even_sig_input.end, even_mux.anchor("l", 0.33), color=even_inp.color)
    arrow(even_adc.edge("r"), even_mux.anchor("l", 0.66), color=even_adc.color)

    # odd
    arrow(odd_pin.anchor("r"), odd_pad, color=odd_pad.color, head_marker="circle")

    arrow(odd_dac.anchor("l"), odd_pad, color=odd_dac.color)
    arrow(odd_out.anchor("l"), odd_pad, color=odd_out.color)

    arrow(
        odd_pad,
        odd_inp.anchor("l", 0.33),
        color=odd_pad.color,
        tail_marker="circle",
        head_label="Pin A",
        fontcolor="#ffff00",
    )
    arrow(even_pad, odd_inp.anchor("l", 0.66), color=even_pad.color, head_label="Pin B", fontcolor="#ffff00")

    arrow(odd_pad, odd_adc.anchor("l"), color=odd_pad.color, head_label="Pin A", fontcolor="#ffff00")

    odd_sig_input = arrow(
        odd_inp.anchor("r"), odd_inp.anchor("r").offset(50, 0), head_marker="circle", color=odd_inp.color
    )
    arrow(odd_sig_input.end, odd_out.anchor("b"), color=odd_inp.color)
    arrow(odd_sig_input.end, odd_mux.anchor("l", 0.33), color=odd_inp.color)

    arrow(odd_adc.anchor("r"), odd_mux.anchor("l", 0.66), color=odd_adc.color)

    # Even Synth Logic

    even_dacsel = box(1006, 197, 151, 235, color="#77933c", label="DAC bus Select (%M...M)")
    even_logout = box(1005, 521, 153, 139, color="#e46c0a", label="Logic Output (%TT) (%SSSSS_0)")
    even_loginp = box(1007, 751, 150, 110, color="#595959", label="Logic Input (%A_B_F)")
    even_smartp = box(1294, 585, 150, 310, color="#953735", label="Even # Smart Pin (%SSSSS_0)")

    # Odd Synth Logic

    odd_dacsel = box(1006, 1040, 151, 235, color="#77933c", label="DAC bus Select (%M...M)")
    odd_logout = box(1005, 1363, 153, 139, color="#e46c0a", label="Logic Output (%TT) (%SSSSS_0)")
    odd_loginp = box(1007, 1594, 150, 110, color="#595959", label="Logic Input (%A_B_F)")
    odd_smartp = box(1294, 1428, 150, 310, color="#953735", label="Odd # Smart Pin (%SSSSS_0)")

    # Core Logic Interconnect

    right_edge = Line(x1=1531, y1=0, x2=1531, y2=1800)

    # even

    for i in range(8):
        pt = even_dacsel.anchor("r", 0.1 * (i + 1))
        arrow(right_edge, pt, color="black", tail_label=f"Cog {i}")

    arrow(right_edge, even_logout.anchor("t"), color="black", tail_label="RND")
    even_sig_out = arrow(right_edge, even_logout.anchor("r", 0.15), color="black", tail_label="OUT")
    even_sig_dir = arrow(right_edge, even_logout.anchor("r", 0.3), color="black", tail_label="DIR")

    arrow(even_smartp.anchor("l", 0.1), even_dacsel.anchor("r", 0.9), color=even_smartp.color)
    arrow(even_smartp.anchor("l", 0.2), even_logout.edge("r"), color=even_smartp.color, label="SmartOut")

    even_smart_a = arrow(even_loginp.anchor("r", 0.33), even_smartp.edge("l"), color=even_loginp.color, label="Smart A")
    arrow(even_loginp.anchor("r", 0.66), even_smartp.edge("l"), color=even_loginp.color, label="Smart B")

    arrow(right_edge, even_smartp.anchor("r", 0.2), color="black", head_label="WXPIN")
    arrow(right_edge, even_smartp.anchor("r", 0.3), color="black", head_label="WYPIN")
    arrow(even_smartp.anchor("r", 0.4), right_edge, color="black", tail_label="RDPIN")
    even_sig_in = arrow(
        replace(even_smartp.anchor("r", 0.6), y=even_smart_a.y2), right_edge, color=even_smartp.color, head_label="IN"
    )
    arrow(right_edge, even_smartp.anchor("r", 0.8), color="black", head_label="ACK")

    arrow(even_smart_a.end, even_sig_in.start, color="white", fontcolor="white", label="(S == 0)")

    for i in range(1, 4):
        arrow(
            even_loginp.anchor("t", 0.25 * i).offset(0, -40),
            even_loginp.anchor("t", 0.25 * i),
            tail_label=f"- {i}",
            color="black",
        )

        arrow(
            even_loginp.anchor("b", 0.25 * i).offset(0, 40),
            even_loginp.anchor("b", 0.25 * i),
            tail_label=f"+ {i}",
            color="black",
        )

    arrow(even_sig_out.at(0.7), even_loginp.anchor("l", 0.33), tail_marker="circle", color="black")

    arrow(even_sig_dir.at(0.1), even_smartp.anchor("r", 0.1), tail_marker="circle", color="black")

    even_sig_enable = arrow(even_logout.anchor("l", 0.25), even_out.edge("r"), color=even_logout.color, label="Enable")
    arrow(even_logout.anchor("l", 0.5), even_out.edge("r"), color=even_logout.color, label="Output")

    arrow(even_sig_enable.at(0.8), even_dac.anchor("r", 0.75), tail_marker="circle", color=even_logout.color)

    even_sig_dacbus = arrow(even_dacsel.edge("l"), even_dac.anchor("r"), label="DACBUS", color=even_dacsel.color)

    arrow(even_sig_dacbus.at(0.8), even_inp.anchor("r", 0.33), tail_marker="circle", color=even_dacsel.color)

    # odd

    # I/O to Core Interconnect

    # even

    arrow(even_mux.anchor("r"), even_loginp.edge("l"), color=even_mux.color)

    # odd

    return elements


def main():
    items = render()

    root = Element("svg", xmlns="http://www.w3.org/2000/svg", width="1998", height="1800")

    style = SubElement(root, "style")
    style.text = """
        text {
            font-family: sans-serif;
            font-size: 10pt;
        }
        rect {
            stroke-width: 3pt;
        }
        line {
            stroke-width: 3pt;
            stroke-linecap: square;
        }
    """

    defs = SubElement(root, "defs")

    def add_marker(_id: str, path: str) -> None:
        marker = SubElement(
            defs,
            "marker",
            id=_id,
            viewBox="0 0 10 10",
            refX="5",
            refY="5",
            markerWidth="5",
            markerHeight="5",
            orient="auto-start-reverse",
        )
        SubElement(marker, "path", d=path)

    add_marker("arrow", "m 0 2 l 5 3 l -5 3 l 0 -1 l 3 -2 l -3 -2 z")
    add_marker("circle", "M 5 2 A 1 1 0 0 0 5 8 A 1 1 0 0 0 5 2")

    for item in items:
        item.render(root)

    print(
        render_xml(
            root,
            pretty_print=True,
        ).decode()
    )


if __name__ == "__main__":
    main()
