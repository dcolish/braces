# coding: utf-8

"""
Unit tests of template.py.

"""

import os
import sys
import unittest

from examples.simple import Simple
from braces import Renderer
from braces import TemplateSpec
from braces.common import TemplateNotFoundError
from braces.context import ContextStack
from braces.parser import ParsingError


from tests.common import (
    get_data_path,
    AssertStringMixin,
    AssertExceptionMixin,
    Attachable,
)
from tests.data.views import NestedObject, SayHello


def _make_renderer():
    """
    Return a default Renderer instance for testing purposes.

    """
    renderer = Renderer(string_encoding='ascii', file_encoding='ascii')
    return renderer


def mock_unicode(b, encoding=None):
    if encoding is None:
        encoding = 'ascii'
    u = unicode(b, encoding=encoding)
    return u.upper()


class RendererInitTestCase(unittest.TestCase):

    """
    Tests the Renderer.__init__() method.

    """

    def test_partials__default(self):
        """
        Test the default value.

        """
        renderer = Renderer()
        self.assertTrue(renderer.partials is None)

    def test_partials(self):
        """
        Test that the attribute is set correctly.

        """
        renderer = Renderer(partials={'foo': 'bar'})
        self.assertEqual(renderer.partials, {'foo': 'bar'})

    def test_escape__default(self):
        escape = Renderer().escape

        self.assertEqual(escape(">"), "&gt;")
        self.assertEqual(escape('"'), "&quot;")
        # Single quotes are escaped only in Python 3.2 and later.
        if sys.version_info < (3, 2):
            expected = "'"
        else:
            expected = '&#x27;'
        self.assertEqual(escape("'"), expected)

    def test_escape(self):
        escape = lambda s: "**" + s
        renderer = Renderer(escape=escape)
        self.assertEqual(renderer.escape("bar"), "**bar")

    def test_decode_errors__default(self):
        """
        Check the default value.

        """
        renderer = Renderer()
        self.assertEqual(renderer.decode_errors, 'strict')

    def test_decode_errors(self):
        """
        Check that the constructor sets the attribute correctly.

        """
        renderer = Renderer(decode_errors="foo")
        self.assertEqual(renderer.decode_errors, "foo")

    def test_file_encoding__default(self):
        """
        Check the file_encoding default.

        """
        renderer = Renderer()
        self.assertEqual(renderer.file_encoding, renderer.string_encoding)

    def test_file_encoding(self):
        """
        Check that the file_encoding attribute is set correctly.

        """
        renderer = Renderer(file_encoding='foo')
        self.assertEqual(renderer.file_encoding, 'foo')

    def test_file_extension__default(self):
        """
        Check the file_extension default.

        """
        renderer = Renderer()
        self.assertEqual(renderer.file_extension, 'mustache')

    def test_file_extension(self):
        """
        Check that the file_encoding attribute is set correctly.

        """
        renderer = Renderer(file_extension='foo')
        self.assertEqual(renderer.file_extension, 'foo')

    def test_search_dirs__default(self):
        """
        Check the search_dirs default.

        """
        renderer = Renderer()
        self.assertEqual(renderer.search_dirs, [os.curdir])

    def test_search_dirs__string(self):
        """
        Check that the search_dirs attribute is set correctly when a string.

        """
        renderer = Renderer(search_dirs='foo')
        self.assertEqual(renderer.search_dirs, ['foo'])

    def test_search_dirs__list(self):
        """
        Check that the search_dirs attribute is set correctly when a list.

        """
        renderer = Renderer(search_dirs=['foo'])
        self.assertEqual(renderer.search_dirs, ['foo'])

    def test_string_encoding__default(self):
        """
        Check the default value.

        """
        renderer = Renderer()
        self.assertEqual(renderer.string_encoding, sys.getdefaultencoding())

    def test_string_encoding(self):
        """
        Check that the constructor sets the attribute correctly.

        """
        renderer = Renderer(string_encoding="foo")
        self.assertEqual(renderer.string_encoding, "foo")


class RendererTests(unittest.TestCase, AssertStringMixin):

    """Test the Renderer class."""

    def _renderer(self, **kw):
        return Renderer(**kw)

    ## Test Renderer.unicode().

    def test_unicode__string_encoding(self):
        """
        Test that the string_encoding attribute is respected.

        """
        renderer = self._renderer()
        b = u"é".encode('utf-8')

        renderer.string_encoding = "ascii"
        self.assertRaises(UnicodeDecodeError, renderer.unicode, b)

        renderer.string_encoding = "utf-8"
        self.assertEqual(renderer.unicode(b), u"é")

    def test_unicode__decode_errors(self):
        """
        Test that the decode_errors attribute is respected.

        """
        renderer = self._renderer(string_encoding="ascii")
        b = u"déf".encode('utf-8')

        renderer.decode_errors = "ignore"
        self.assertEqual(renderer.unicode(b), "df")

        renderer.decode_errors = "replace"
        # U+FFFD is the official Unicode replacement character.
        self.assertEqual(renderer.unicode(b), u'd\ufffd\ufffdf')

    def test_renderer_loader__attributes(self):
        """
        Test that _make_loader() sets all attributes correctly..

        """
        unicode_ = lambda x: x

        renderer = self._renderer(file_encoding='enc',
                                  file_extension='ext',
                                  to_unicode=unicode_)

        loader = renderer.loader

        self.assertEqual(loader.extension, 'ext')
        self.assertEqual(loader.file_encoding, 'enc')
        self.assertEqual(loader.to_unicode, unicode_)

    ## Test the render() method.

    def test_render__return_type(self):
        """
        Check that render() returns a string of type unicode.

        """
        renderer = self._renderer()
        rendered = renderer.render('foo')
        self.assertEqual(type(rendered), unicode)

    def test_render__unicode(self):
        renderer = self._renderer()
        actual = renderer.render(u'foo')
        self.assertEqual(actual, u'foo')

    def test_render__str(self):
        renderer = self._renderer()
        actual = renderer.render('foo')
        self.assertEqual(actual, 'foo')

    def test_render__non_ascii_character(self):
        renderer = self._renderer()
        actual = renderer.render(u'Poincaré')
        self.assertEqual(actual, u'Poincaré')

    def test_render__context(self):
        """
        Test render(): passing a context.

        """
        renderer = self._renderer()
        self.assertEqual(renderer.render('Hi {{person}}', {'person': 'Mom'}), 'Hi Mom')

    def test_render__context_and_kwargs(self):
        """
        Test render(): passing a context and **kwargs.

        """
        renderer = self._renderer()
        template = 'Hi {{person1}} and {{person2}}'
        self.assertEqual(renderer.render(template, {'person1': 'Mom'}, person2='Dad'), 'Hi Mom and Dad')

    def test_render__kwargs_and_no_context(self):
        """
        Test render(): passing **kwargs and no context.

        """
        renderer = self._renderer()
        self.assertEqual(renderer.render('Hi {{person}}', person='Mom'), 'Hi Mom')

    def test_render__context_and_kwargs__precedence(self):
        """
        Test render(): **kwargs takes precedence over context.

        """
        renderer = self._renderer()
        self.assertEqual(renderer.render('Hi {{person}}', {'person': 'Mom'}, person='Dad'), 'Hi Dad')

    def test_render__kwargs_does_not_modify_context(self):
        """
        Test render(): passing **kwargs does not modify the passed context.

        """
        context = {}
        renderer = self._renderer()
        renderer.render('Hi {{person}}', context=context, foo="bar")
        self.assertEqual(context, {})

    def test_render__nonascii_template(self):
        """
        Test passing a non-unicode template with non-ascii characters.

        """
        renderer = _make_renderer()
        template = u"déf".encode("utf-8")

        # Check that decode_errors and string_encoding are both respected.
        renderer.decode_errors = 'ignore'
        renderer.string_encoding = 'ascii'
        self.assertEqual(renderer.render(template), "df")

        renderer.string_encoding = 'utf_8'
        self.assertEqual(renderer.render(template), u"déf")

    def test_make_load_partial(self):
        """
        Test the _make_load_partial() method.

        """
        renderer = Renderer()
        renderer.partials = {'foo': 'bar'}
        load_partial = renderer._make_load_partial()

        actual = load_partial('foo')
        self.assertEqual(actual, 'bar')
        self.assertEqual(type(actual), unicode, "RenderEngine requires that "
            "load_partial return unicode strings.")

    def test_make_load_partial__unicode(self):
        """
        Test _make_load_partial(): that load_partial doesn't "double-decode" Unicode.

        """
        renderer = Renderer()

        renderer.partials = {'partial': 'foo'}
        load_partial = renderer._make_load_partial()
        self.assertEqual(load_partial("partial"), "foo")

        # Now with a value that is already unicode.
        renderer.partials = {'partial': u'foo'}
        load_partial = renderer._make_load_partial()
        # If the next line failed, we would get the following error:
        #   TypeError: decoding Unicode is not supported
        self.assertEqual(load_partial("partial"), "foo")

    def test_render_path(self):
        """
        Test the render_path() method.

        """
        renderer = Renderer()
        path = get_data_path('say_hello.mustache')
        actual = renderer.render_path(path, to='foo')
        self.assertEqual(actual, "Hello, foo")

    def test_render__object(self):
        """
        Test rendering an object instance.

        """
        renderer = Renderer()

        say_hello = SayHello()
        actual = renderer.render(say_hello)
        self.assertEqual('Hello, World', actual)

        actual = renderer.render(say_hello, to='Mars')
        self.assertEqual('Hello, Mars', actual)

    def test_render__dotted_path(self):
        renderer = Renderer()
        template = "{{meh.foo}}"
        actual = renderer.render(template, NestedObject())
        self.assertEqual(actual, 'bar')

    def test_render__template_spec(self):
        """
        Test rendering a TemplateSpec instance.

        """
        renderer = Renderer()

        class Spec(TemplateSpec):
            template = "hello, {{to}}"
            to = 'world'

        spec = Spec()
        actual = renderer.render(spec)
        self.assertString(actual, u'hello, world')

    def test_render__view(self):
        """
        Test rendering a View instance.

        """
        renderer = Renderer()

        view = Simple()
        actual = renderer.render(view)
        self.assertEqual('Hi pizza!', actual)


# By testing that Renderer.render() constructs the right RenderEngine,
# we no longer need to exercise all rendering code paths through
# the Renderer.  It suffices to test rendering paths through the
# RenderEngine for the same amount of code coverage.
class Renderer_MakeRenderer(unittest.TestCase, AssertExceptionMixin):

    """
    Check the RenderEngine returned by Renderer._make_render_engine().

    """

    def _make_renderer(self):
        """
        Return a default Renderer instance for testing purposes.

        """
        return _make_renderer()

    ## Test the engine's load_partial attribute.

    def test__load_partial__returns_unicode(self):
        """
        Check that load_partial returns unicode (and not a subclass).

        """
        class MyUnicode(unicode):
            pass

        renderer = Renderer(string_encoding='ascii',
                            partials={'str': 'foo',
                                      'subclass': MyUnicode('abc')})

        actual = renderer.load_partial('str')
        self.assertEqual(actual, "foo")
        self.assertEqual(type(actual), unicode)

        # Check that unicode subclasses are not preserved.
        actual = renderer.load_partial('subclass')
        self.assertEqual(actual, "abc")
        self.assertEqual(type(actual), unicode)

    def test__load_partial__not_found__default(self):
        """
        Check that load_partial provides a nice message when a template is not found.

        """
        renderer = Renderer()
        load_partial = renderer.load_partial

        self.assertException(TemplateNotFoundError, "File 'foo.mustache' not found in dirs: ['.']",
                             load_partial, "foo")

    def test__load_partial__not_found__dict(self):
        """
        Check that load_partial provides a nice message when a template is not found.

        """
        renderer = Renderer(partials={})

        load_partial = renderer.load_partial

        # Include dict directly since str(dict) is different in Python 2 and 3:
        #   <type 'dict'> versus <class 'dict'>, respectively.
        self.assertException(TemplateNotFoundError, "Name 'foo' not found in partials: %s" % dict,
                             load_partial, "foo")

    ## Test the engine's literal attribute.

    def test__literal__uses_renderer_unicode(self):
        """
        Test that literal uses the renderer's unicode function.

        """
        renderer = self._make_renderer()
        renderer.unicode = mock_unicode

        literal = renderer.literal

        b = u"foo".encode("ascii")
        self.assertEqual(literal(b), "FOO")

    def test__literal__handles_unicode(self):
        """
        Test that literal doesn't try to "double decode" unicode.

        """
        renderer = Renderer(string_encoding='ascii')

        literal = renderer.literal

        self.assertEqual(literal(u"foo"), "foo")

    def test__literal__returns_unicode(self):
        """
        Test that literal returns unicode (and not a subclass).

        """
        renderer = Renderer(string_encoding='ascii')
        literal = renderer.literal

        self.assertEqual(type(literal("foo")), unicode)

        class MyUnicode(unicode):
            pass

        s = MyUnicode("abc")

        self.assertEqual(type(s), MyUnicode)
        self.assertTrue(isinstance(s, unicode))
        self.assertEqual(type(literal(s)), unicode)

    ## Test the engine's escape attribute.

    def test__escape__uses_renderer_escape(self):
        """
        Test that escape uses the renderer's escape function.

        """
        renderer = Renderer(escape=lambda s: "**" + s)
        escape = renderer.escape

        self.assertEqual(escape("foo"), "**foo")

    def test__escape__uses_renderer_unicode(self):
        """
        Test that escape uses the renderer's unicode function.

        """
        renderer = Renderer()
        renderer.unicode = mock_unicode
        escape = renderer.escape

        b = u"foo".encode('ascii')
        self.assertEqual(escape(b), "FOO")

    def test__escape__has_access_to_original_unicode_subclass(self):
        """
        Test that escape receives strings with the unicode subclass intact.

        """
        renderer = Renderer(escape=lambda s: unicode(type(s).__name__))
        escape = renderer.escape

        class MyUnicode(unicode):
            pass

        self.assertEqual(escape(u"foo".encode('ascii')), unicode.__name__)
        self.assertEqual(escape(u"foo"), unicode.__name__)
        self.assertEqual(escape(MyUnicode("foo")), MyUnicode.__name__)

    def test__escape__returns_unicode(self):
        """
        Test that literal returns unicode (and not a subclass).

        """
        renderer = Renderer(string_encoding='ascii')
        escape = renderer.escape

        self.assertEqual(type(escape("foo")), unicode)

        # Check that literal doesn't preserve unicode subclasses.
        class MyUnicode(unicode):
            pass

        s = MyUnicode("abc")

        self.assertEqual(type(s), MyUnicode)
        self.assertTrue(isinstance(s, unicode))
        self.assertEqual(type(escape(s)), unicode)


def mock_literal(s):
    """
    For use as the literal keyword argument to the RenderEngine constructor.

    Arguments:

      s: a byte string or unicode string.

    """
    if isinstance(s, unicode):
        # Strip off unicode super classes, if present.
        u = unicode(s)
    else:
        u = unicode(s, encoding='ascii')

    # We apply upper() to make sure we are actually using our custom
    # function in the tests
    return u.upper()


class RenderTests(unittest.TestCase, AssertStringMixin):

    """
    Tests RenderEngine.render().

    Explicit spec-test-like tests best go in this class since the
    RenderEngine class contains all parsing logic.  This way, the unit tests
    will be more focused and fail "closer to the code".

    """

    def _assert_render(self, expected, template, *context, **kwargs):
        """
        Test rendering the given template using the given context.

        """
        partials = kwargs.get('partials', None)
        engine = kwargs.get('engine', None)

        if not engine:
            engine = Renderer(partials=partials)
        context = ContextStack(*context)

        actual = engine.render(template, context)

        self.assertString(actual=actual, expected=expected)

    def test_render(self):
        self._assert_render(u'Hi Mom', 'Hi {{person}}', {'person': 'Mom'})

    def test__load_partial(self):
        """
        Test that render() uses the load_template attribute.

        """
        partials = {'partial': u"{{person}}"}
        engine = Renderer(partials=partials)

        self._assert_render(u'Hi Mom', 'Hi {{>partial}}', {'person': 'Mom'}, engine=engine)

    def test__literal(self):
        """
        Test that render() uses the literal attribute.

        """
        engine = Renderer(literal=lambda s: s.upper())

        self._assert_render(u'BAR', '{{{foo}}}', {'foo': 'bar'}, engine=engine)

    def test_literal__sigil(self):
        template = "<h1>{{& thing}}</h1>"
        context = {'thing': 'Bear > Giraffe'}

        expected = u"<h1>Bear > Giraffe</h1>"

        self._assert_render(expected, template, context)

    def test__escape(self):
        """
        Test that render() uses the escape attribute.

        """
        engine = Renderer(escape=lambda s: "**" + s)

        self._assert_render(u'**bar', '{{foo}}', {'foo': 'bar'}, engine=engine)

    def test__escape_does_not_call_literal(self):
        """
        Test that render() does not call literal before or after calling escape.

        """
        engine = Renderer(literal=lambda s: s.upper(),
                          escape=lambda s: "**" + s)

        template = 'literal: {{{foo}}} escaped: {{foo}}'
        context = {'foo': 'bar'}

        self._assert_render(u'literal: BAR escaped: **bar', template, context, engine=engine)

    def test__escape_preserves_unicode_subclasses(self):
        """
        Test that render() preserves unicode subclasses when passing to escape.

        This is useful, for example, if one wants to respect whether a
        variable value is markupsafe.Markup when escaping.

        """
        class MyUnicode(unicode):
            pass

        def escape(s):
            if type(s) is MyUnicode:
                return "**" + s
            else:
                return s + "**"

        engine = Renderer(escape=escape)

        template = '{{foo1}} {{foo2}}'
        context = {'foo1': MyUnicode('bar'), 'foo2': 'bar'}

        self._assert_render(u'**bar bar**', template, context, engine=engine)

    def test__non_basestring__literal_and_escaped(self):
        """
        Test a context value that is not a basestring instance.

        """
        engine = Renderer(escape=mock_literal,
                          literal=mock_literal)

        self.assertRaises(TypeError, engine.literal, 100)

        template = '{{text}} {{int}} {{{int}}}'
        context = {'int': 100, 'text': 'foo'}

        self._assert_render(u'FOO 100 100', template, context, engine=engine)

    def test_tag__output_not_interpolated(self):
        """
        Context values should not be treated as templates (issue #44).

        """
        template = '{{template}}: {{planet}}'
        context = {'template': '{{planet}}', 'planet': 'Earth'}
        self._assert_render(u'{{planet}}: Earth', template, context)

    def test_tag__output_not_interpolated__section(self):
        """
        Context values should not be treated as templates (issue #44).

        """
        template = '{{test}}'
        context = {'test': '{{#hello}}'}
        self._assert_render(u'{{#hello}}', template, context)

    ## Test interpolation with "falsey" values
    #
    # In these test cases, we test the part of the spec that says that
    # "data should be coerced into a string (and escaped, if appropriate)
    # before interpolation."  We test this for data that is "falsey."

    def test_interpolation__falsey__zero(self):
        template = '{{.}}'
        context = 0
        self._assert_render(u'0', template, context)

    def test_interpolation__falsey__none(self):
        template = '{{.}}'
        context = None
        self._assert_render(u'None', template, context)

    def test_interpolation__falsey__False(self):
        template = '{{.}}'
        context = False
        self._assert_render(u'False', template, context)

    # Built-in types:
    #
    #   Confirm that we not treat instances of built-in types as objects,
    #   for example by calling a method on a built-in type instance when it
    #   has a method whose name matches the current key.
    #
    #   Each test case puts an instance of a built-in type on top of the
    #   context stack before interpolating a tag whose key matches an
    #   attribute (method or property) of the instance.
    #

    def _assert_builtin_attr(self, item, attr_name, expected_attr):
        self.assertTrue(hasattr(item, attr_name))
        actual = getattr(item, attr_name)
        if callable(actual):
            actual = actual()
        self.assertEqual(actual, expected_attr)

    def _assert_builtin_type(self, item, attr_name, expected_attr, expected_template):
        self._assert_builtin_attr(item, attr_name, expected_attr)

        template = '{{#section}}{{%s}}{{/section}}' % attr_name
        context = {'section': item, attr_name: expected_template}
        self._assert_render(expected_template, template, context)

    def test_interpolation__built_in_type__string(self):
        """
        Check tag interpolation with a built-in type: string.

        """
        self._assert_builtin_type('abc', 'upper', 'ABC', u'xyz')

    def test_interpolation__built_in_type__integer(self):
        """
        Check tag interpolation with a built-in type: integer.

        """
        # Since public attributes weren't added to integers until Python 2.6
        # (for example the "real" attribute of the numeric type hierarchy)--
        #
        #   http://docs.python.org/library/numbers.html
        #
        # we need to resort to built-in attributes (double-underscored) on
        # the integer type.
        self._assert_builtin_type(15, '__neg__', -15, u'999')

    def test_interpolation__built_in_type__list(self):
        """
        Check tag interpolation with a built-in type: list.

        """
        item = [[1, 2, 3]]
        attr_name = 'pop'
        # Make a copy to prevent changes to item[0].
        self._assert_builtin_attr(list(item[0]), attr_name, 3)

        template = '{{#section}}{{%s}}{{/section}}' % attr_name
        context = {'section': item, attr_name: 7}
        self._assert_render(u'7', template, context)

    def test_implicit_iterator__literal(self):
        """
        Test an implicit iterator in a literal tag.

        """
        template = """{{#test}}{{{.}}}{{/test}}"""
        context = {'test': ['<', '>']}

        self._assert_render(u'<>', template, context)

    def test_implicit_iterator__escaped(self):
        """
        Test an implicit iterator in a normal tag.

        """
        template = """{{#test}}{{.}}{{/test}}"""
        context = {'test': ['<', '>']}

        self._assert_render(u'&lt;&gt;', template, context)

    def test_literal__in_section(self):
        """
        Check that literals work in sections.

        """
        template = '{{#test}}1 {{{less_than}}} 2{{/test}}'
        context = {'test': {'less_than': '<'}}

        self._assert_render(u'1 < 2', template, context)

    def test_literal__in_partial(self):
        """
        Check that literals work in partials.

        """
        template = '{{>partial}}'
        partials = {'partial': '1 {{{less_than}}} 2'}
        context = {'less_than': '<'}

        self._assert_render(u'1 < 2', template, context, partials=partials)

    def test_partial(self):
        partials = {'partial': "{{person}}"}
        self._assert_render(u'Hi Mom', 'Hi {{>partial}}', {'person': 'Mom'}, partials=partials)

    def test_partial__context_values(self):
        """
        Test that escape and literal work on context values in partials.

        """
        template = '{{>partial}}'
        partials = {'partial': 'unescaped: {{{foo}}} escaped: {{foo}}'}
        context = {'foo': '<'}

        self._assert_render(u'unescaped: < escaped: &lt;', template, context, partials=partials)

    ## Test cases related specifically to sections.

    def test_section__end_tag_with_no_start_tag(self):
        """
        Check what happens if there is an end tag with no start tag.

        """
        template = '{{/section}}'
        try:
            self._assert_render(None, template)
        except ParsingError, err:
            self.assertEqual(str(err), "Section end tag mismatch: section != None")

    def test_section__end_tag_mismatch(self):
        """
        Check what happens if the end tag doesn't match.

        """
        template = '{{#section_start}}{{/section_end}}'
        try:
            self._assert_render(None, template)
        except ParsingError, err:
            self.assertEqual(str(err), "Section end tag mismatch: section_end != section_start")

    def test_section__context_values(self):
        """
        Test that escape and literal work on context values in sections.

        """
        template = '{{#test}}unescaped: {{{foo}}} escaped: {{foo}}{{/test}}'
        context = {'test': {'foo': '<'}}

        self._assert_render(u'unescaped: < escaped: &lt;', template, context)

    def test_section__context_precedence(self):
        """
        Check that items higher in the context stack take precedence.

        """
        template = '{{entree}} : {{#vegetarian}}{{entree}}{{/vegetarian}}'
        context = {'entree': 'chicken', 'vegetarian': {'entree': 'beans and rice'}}
        self._assert_render(u'chicken : beans and rice', template, context)

    def test_section__list_referencing_outer_context(self):
        """
        Check that list items can access the parent context.

        For sections whose value is a list, check that items in the list
        have access to the values inherited from the parent context
        when rendering.

        """
        context = {
            "greeting": "Hi",
            "list": [{"name": "Al"}, {"name": "Bob"}],
        }

        template = "{{#list}}{{greeting}} {{name}}, {{/list}}"

        self._assert_render(u"Hi Al, Hi Bob, ", template, context)

    def test_section__output_not_interpolated(self):
        """
        Check that rendered section output is not interpolated.

        """
        template = '{{#section}}{{template}}{{/section}}: {{planet}}'
        context = {'section': True, 'template': '{{planet}}', 'planet': 'Earth'}
        self._assert_render(u'{{planet}}: Earth', template, context)

    # TODO: have this test case added to the spec.
    def test_section__string_values_not_lists(self):
        """
        Check that string section values are not interpreted as lists.

        """
        template = '{{#section}}foo{{/section}}'
        context = {'section': '123'}
        # If strings were interpreted as lists, this would give "foofoofoo".
        self._assert_render(u'foo', template, context)

    def test_section__nested_truthy(self):
        """
        Check that "nested truthy" sections get rendered.

        Test case for issue #24: https://github.com/defunkt/pystache/issues/24

        This test is copied from the spec.  We explicitly include it to
        prevent regressions for those who don't pull down the spec tests.

        """
        template = '| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |'
        context = {'bool': True}
        self._assert_render(u'| A B C D E |', template, context)

    def test_section__nested_with_same_keys(self):
        """
        Check a doubly-nested section with the same context key.

        Test case for issue #36: https://github.com/defunkt/pystache/issues/36

        """
        # Start with an easier, working case.
        template = '{{#x}}{{#z}}{{y}}{{/z}}{{/x}}'
        context = {'x': {'z': {'y': 1}}}
        self._assert_render(u'1', template, context)

        template = '{{#x}}{{#x}}{{y}}{{/x}}{{/x}}'
        context = {'x': {'x': {'y': 1}}}
        self._assert_render(u'1', template, context)

    def test_section__lambda(self):
        template = '{{#test}}Mom{{/test}}'
        context = {'test': (lambda text: 'Hi %s' % text)}
        self._assert_render(u'Hi Mom', template, context)

    def test_section__iterable(self):
        """
        Check that objects supporting iteration (aside from dicts) behave like lists.

        """
        template = '{{#iterable}}{{.}}{{/iterable}}'

        context = {'iterable': (i for i in range(3))}  # type 'generator'
        self._assert_render(u'012', template, context)

        context = {'iterable': xrange(4)}  # type 'xrange'
        self._assert_render(u'0123', template, context)

        d = {'foo': 0, 'bar': 0}
        # We don't know what order of keys we'll be given, but from the
        # Python documentation:
        #  "If items(), keys(), values(), iteritems(), iterkeys(), and
        #   itervalues() are called with no intervening modifications to
        #   the dictionary, the lists will directly correspond."
        expected = u''.join(d.keys())
        context = {'iterable': d.iterkeys()}  # type 'dictionary-keyiterator'
        self._assert_render(expected, template, context)

    def test_section__lambda__tag_in_output(self):
        """
        Check that callable output is treated as a template string (issue #46).

        The spec says--

            When used as the data value for a Section tag, the lambda MUST
            be treatable as an arity 1 function, and invoked as such (passing
            a String containing the unprocessed section contents).  The
            returned value MUST be rendered against the current delimiters,
            then interpolated in place of the section.

        """
        template = '{{#test}}Hi {{person}}{{/test}}'
        context = {'person': 'Mom', 'test': (lambda text: text + " :)")}
        self._assert_render(u'Hi Mom :)', template, context)

    def test_section__lambda__list(self):
        """
        Check that lists of lambdas are processed correctly for sections.

        This test case is equivalent to a test submitted to the Mustache spec here:

          https://github.com/mustache/spec/pull/47 .

        """
        template = '<{{#lambdas}}foo{{/lambdas}}>'
        context = {'foo': 'bar',
                   'lambdas': [lambda text: "~{{%s}}~" % text,
                               lambda text: "#{{%s}}#" % text]}

        self._assert_render(u'<~bar~#bar#>', template, context)

    def test_section__lambda__mixed_list(self):
        """
        Test a mixed list of lambdas and non-lambdas as a section value.

        This test case is equivalent to a test submitted to the Mustache spec here:

          https://github.com/mustache/spec/pull/47 .

        """
        template = '<{{#lambdas}}foo{{/lambdas}}>'
        context = {'foo': 'bar',
                   'lambdas': [lambda text: "~{{%s}}~" % text, 1]}

        self._assert_render(u'<~bar~foo>', template, context)

    def test_section__lambda__not_on_context_stack(self):
        """
        Check that section lambdas are not pushed onto the context stack.

        Even though the sections spec says that section data values should be
        pushed onto the context stack prior to rendering, this does not apply
        to lambdas.  Lambdas obey their own special case.

        This test case is equivalent to a test submitted to the Mustache spec here:

          https://github.com/mustache/spec/pull/47 .

        """
        context = {'foo': 'bar', 'lambda': (lambda text: "{{.}}")}
        template = '{{#foo}}{{#lambda}}blah{{/lambda}}{{/foo}}'
        self._assert_render(u'bar', template, context)

    def test_section__lambda__no_reinterpolation(self):
        """
        Check that section lambda return values are not re-interpolated.

        This test is a sanity check that the rendered lambda return value
        is not re-interpolated as could be construed by reading the
        section part of the Mustache spec.

        This test case is equivalent to a test submitted to the Mustache spec here:

          https://github.com/mustache/spec/pull/47 .

        """
        template = '{{#planet}}{{#lambda}}dot{{/lambda}}{{/planet}}'
        context = {'planet': 'Earth', 'dot': '~{{.}}~', 'lambda': (lambda text: "#{{%s}}#" % text)}
        self._assert_render(u'#~{{.}}~#', template, context)

    def test_comment__multiline(self):
        """
        Check that multiline comments are permitted.

        """
        self._assert_render(u'foobar', 'foo{{! baz }}bar')
        self._assert_render(u'foobar', 'foo{{! \nbaz }}bar')

    def test_custom_delimiters__sections(self):
        """
        Check that custom delimiters can be used to start a section.

        Test case for issue #20: https://github.com/defunkt/pystache/issues/20

        """
        template = '{{=[[ ]]=}}[[#foo]]bar[[/foo]]'
        context = {'foo': True}
        self._assert_render(u'bar', template, context)

    def test_custom_delimiters__not_retroactive(self):
        """
        Check that changing custom delimiters back is not "retroactive."

        Test case for issue #35: https://github.com/defunkt/pystache/issues/35

        """
        expected = u' {{foo}} '
        self._assert_render(expected, '{{=$ $=}} {{foo}} ')
        self._assert_render(expected, '{{=$ $=}} {{foo}} $={{ }}=$')  # was yielding u'  '.

    def test_dot_notation(self):
        """
        Test simple dot notation cases.

        Check that we can use dot notation when the variable is a dict,
        user-defined object, or combination of both.

        """
        template = 'Hello, {{person.name}}. I see you are {{person.details.age}}.'
        person = Attachable(name='Biggles', details={'age': 42})
        context = {'person': person}
        self._assert_render(u'Hello, Biggles. I see you are 42.', template, context)

    def test_dot_notation__missing_attributes_or_keys(self):
        """
        Test dot notation with missing keys or attributes.

        Check that if a key or attribute in a dotted name does not exist, then
        the tag renders as the empty string.

        """
        template = """I cannot see {{person.name}}'s age: {{person.age}}.
        Nor {{other_person.name}}'s: ."""
        expected = u"""I cannot see Biggles's age: .
        Nor Mr. Bradshaw's: ."""
        context = {'person': {'name': 'Biggles'},
                   'other_person': Attachable(name='Mr. Bradshaw')}
        self._assert_render(expected, template, context)

    def test_dot_notation__multiple_levels(self):
        """
        Test dot notation with multiple levels.

        """
        template = """Hello, Mr. {{person.name.lastname}}.
        I see you're back from {{person.travels.last.country.city}}.
        I'm missing some of your details: {{person.details.private.editor}}."""
        expected = u"""Hello, Mr. Pither.
        I see you're back from Cornwall.
        I'm missing some of your details: ."""
        context = {'person': {'name': {'firstname': 'unknown', 'lastname': 'Pither'},
                            'travels': {'last': {'country': {'city': 'Cornwall'}}},
                            'details': {'public': 'likes cycling'}}}
        self._assert_render(expected, template, context)

        # It should also work with user-defined objects
        context = {'person': Attachable(name={'firstname': 'unknown', 'lastname': 'Pither'},
                                        travels=Attachable(last=Attachable(country=Attachable(city='Cornwall'))),
                                        details=Attachable())}
        self._assert_render(expected, template, context)

    def test_dot_notation__missing_part_terminates_search(self):
        """
        Test that dotted name resolution terminates on a later part not found.

        Check that if a later dotted name part is not found in the result from
        the former resolution, then name resolution terminates rather than
        starting the search over with the next element of the context stack.
        From the spec (interpolation section)--

          5) If any name parts were retained in step 1, each should be resolved
          against a context stack containing only the result from the former
          resolution.  If any part fails resolution, the result should be considered
          falsey, and should interpolate as the empty string.

        This test case is equivalent to the test case in the following pull
        request:

          https://github.com/mustache/spec/pull/48

        """
        template = '{{a.b}} :: ({{#c}}{{a}} :: {{a.b}}{{/c}})'
        context = {'a': {'b': 'A.B'}, 'c': {'a': 'A'} }
        self._assert_render(u'A.B :: (A :: )', template, context)
