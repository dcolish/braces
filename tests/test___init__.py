# coding: utf-8

"""
Tests of __init__.py.

"""

# Calling "import *" is allowed only at the module level.
GLOBALS_INITIAL = globals().keys()
from braces import *
GLOBALS_PYSTACHE_IMPORTED = globals().keys()

import unittest

import braces


class InitTests(unittest.TestCase):

    def test___all__(self):
        """
        Test that "from braces import *" works as expected.

        """
        actual = set(GLOBALS_PYSTACHE_IMPORTED) - set(GLOBALS_INITIAL)
        expected = set(['render', 'Renderer', 'TemplateSpec', 'GLOBALS_INITIAL'])

        self.assertEqual(actual, expected)

    def test_version_defined(self):
        """
        Test that braces.__version__ is set.

        """
        actual_version = braces.__version__
        self.assertTrue(actual_version)
