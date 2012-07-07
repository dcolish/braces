# coding: utf-8

"""
TODO: add a docstring.

"""

from braces import TemplateSpec

class SayHello(object):

    def to(self):
        return "World"


class SampleView(TemplateSpec):
    pass


class NonAscii(TemplateSpec):
    pass


class SomeObject(object):
    foo = 'bar'


class NestedObject(object):
    meh = SomeObject()
