"""Regression checks for executor preservation and compile-time placeholder guards."""
import io
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

import gen_windtunnel as generator
from common import P2INSTRUCTIONS_JSON, P2INSTRUCTIONS_TSV, decode_dataset

ROOT = Path(__file__).resolve().parent.parent


class ExecutorGeneration(unittest.TestCase):
    """Keep semantic sections intact and reject freshly generated instruction bodies."""

    @classmethod
    def setUpClass(cls):
        """Render the real instruction dataset without any preserved implementations."""
        cls.instructions = [item for item in decode_dataset(P2INSTRUCTIONS_JSON, P2INSTRUCTIONS_TSV)
                            if not item.is_alias]
        cls.groups = generator.process_groups()
        output = io.StringIO()
        generator.render_executor_stub(output, cls.instructions, cls.groups)
        cls.rendered = output.getvalue()

    def test_no_runtime_missing_implementation_fallback(self):
        """Missing semantic bodies must emit a compiler guard, never a simulator outcome."""
        self.assertNotIn(".not_implemented", self.rendered)
        self.assertIn('@compileError("missing instruction body:', self.rendered)
        self.assertIn(".invalid => return cog.reject(.invalid_encoding)", self.rendered)

    def test_preserves_every_existing_semantic_section(self):
        """Round-trip all saved instruction bodies, runtime helpers and executor tests."""
        original = (ROOT / "src/windtunnel/sim/execute.zig").read_text()
        _, sections = generator._exchange_patches(original, None)
        regenerated, _ = generator._exchange_patches(self.rendered, sections)
        _, actual_sections = generator._exchange_patches(regenerated, None)
        self.assertEqual(sections, actual_sections)

    def test_instantiated_placeholder_fails_compilation(self):
        """Instantiate an actual generated placeholder and verify its deliberate compiler error."""
        manifest = (ROOT / "build.zig.zon").read_text()
        version = re.search(r'minimum_zig_version = "([^"]+)"', manifest).group(1)
        binary = shutil.which("zig-" + version)
        if binary is None:
            self.skipTest("versioned project Zig binary is unavailable")
        declaration = re.search(r'pub fn (\w+)\(cog: \*Cog, args: encoding\.(\w+)\) Cog.ExecResult \{.*?\n\}',
                                self.rendered, re.S)
        self.assertIsNotNone(declaration)
        name, group = declaration.group(1, 2)
        source = ("const Cog = struct { const ExecResult = enum { next, wait, skip }; };\n"
                  "const encoding = struct { const " + group + " = struct {}; };\n" +
                  declaration.group(0) + "\ntest { _ = " + name + "(undefined, .{}); }\n")
        with tempfile.TemporaryDirectory(prefix="windtunnel-executor-") as directory:
            path = Path(directory) / "placeholder.zig"
            path.write_text(source)
            result = subprocess.run([binary, "test", str(path), "-fno-emit-bin"],
                                    text=True, capture_output=True)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("missing instruction body:", result.stderr)


if __name__ == "__main__":
    unittest.main()

