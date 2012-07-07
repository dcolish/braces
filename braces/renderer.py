# coding: utf-8

"""
This module provides a Renderer class to render templates.

"""

import sys

from braces import defaults
from braces.common import TemplateNotFoundError
from braces.context import ContextStack
from braces.loader import Loader
from braces.parser import Parser
from braces.specloader import SpecLoader
from braces.template_spec import TemplateSpec


# TODO: come up with a better solution for this.  One of the issues here
#   is that in Python 3 there is no common base class for unicode strings
#   and byte strings, and 2to3 seems to convert all of "str", "unicode",
#   and "basestring" to Python 3's "str".
if sys.version_info < (3, ):
    _STRING_TYPES = basestring
else:
    # The latter evaluates to "bytes" in Python 3 -- even after conversion by 2to3.
    _STRING_TYPES = (unicode, type(u"a".encode('utf-8')))


class Renderer(object):

    """
    A class for rendering mustache templates.

    This class supports several rendering options which are described in
    the constructor's docstring.  Among these, the constructor supports
    passing a custom partial loader.

    Here is an example of rendering a template using a custom partial loader
    that loads partials from a string-string dictionary.

    >>> partials = {'partial': 'Hello, {{thing}}!'}
    >>> renderer = Renderer(partials=partials)
    >>> # We apply print to make the test work in Python 3 after 2to3.
    >>> print renderer.render('{{>partial}}', {'thing': 'world'})
    Hello, world!

    """

    def __init__(self, file_encoding=None, string_encoding=None,
                 decode_errors=None, search_dirs=None, file_extension=None,
                 escape=None, partials=None, to_unicode=None):
        """
        Construct an instance.

        Arguments:

          partials: an object (e.g. a dictionary) for custom partial loading
            during the rendering process.
                The object should have a get() method that accepts a string
            and returns the corresponding template as a string, preferably
            as a unicode string.  If there is no template with that name,
            the get() method should either return None (as dict.get() does)
            or raise an exception.
                If this argument is None, the rendering process will use
            the normal procedure of locating and reading templates from
            the file system -- using relevant instance attributes like
            search_dirs, file_encoding, etc.

          decode_errors: the string to pass as the errors argument to the
            built-in function unicode() when converting str strings to
            unicode.  Defaults to the package default.

          escape: the function used to escape variable tag values when
            rendering a template.  The function should accept a unicode
            string (or subclass of unicode) and return an escaped string
            that is again unicode (or a subclass of unicode).
                This function need not handle strings of type `str` because
            this class will only pass it unicode strings.  The constructor
            assigns this function to the constructed instance's escape()
            method.
                To disable escaping entirely, one can pass `lambda u: u`
            as the escape function, for example.  One may also wish to
            consider using markupsafe's escape function: markupsafe.escape().
            This argument defaults to the package default.

          file_encoding: the name of the default encoding to use when reading
            template files.  All templates are converted to unicode prior
            to parsing.  This encoding is used when reading template files
            and converting them to unicode.  Defaults to the package default.

          file_extension: the template file extension.  Pass False for no
            extension (i.e. to use extensionless template files).
            Defaults to the package default.

          search_dirs: the list of directories in which to search when
            loading a template by name or file name.  If given a string,
            the method interprets the string as a single directory.
            Defaults to the package default.

          string_encoding: the name of the encoding to use when converting
            to unicode any strings of type str encountered during the
            rendering process.  The name will be passed as the encoding
            argument to the built-in function unicode().  Defaults to the
            package default.

        """
        if decode_errors is None:
            decode_errors = defaults.DECODE_ERRORS

        if escape is None:
            escape = defaults.TAG_ESCAPE

        if file_encoding is None:
            file_encoding = defaults.FILE_ENCODING

        if file_extension is None:
            file_extension = defaults.TEMPLATE_EXTENSION

        if search_dirs is None:
            search_dirs = defaults.SEARCH_DIRS

        if string_encoding is None:
            string_encoding = defaults.STRING_ENCODING

        if isinstance(search_dirs, basestring):
            search_dirs = [search_dirs]

        if to_unicode is None:
            to_unicode = self.unicode

        self._context = None
        self.decode_errors = decode_errors
        self._escape = escape

        self.file_encoding = file_encoding
        self.file_extension = file_extension
        self.literal = self._to_unicode_hard
        self.loader = Loader(file_encoding=file_encoding,
                             extension=file_extension,
                             to_unicode=to_unicode,
                             search_dirs=search_dirs)
        self.partials = partials
        self.search_dirs = search_dirs
        self.string_encoding = string_encoding
        self.parser = Parser(self)

        self.load_partial = self._make_load_partial()

    # This is an experimental way of giving views access to the current context.
    # TODO: consider another approach of not giving access via a property,
    #   but instead letting the caller pass the initial context to the
    #   main render() method by reference.  This approach would probably
    #   be less likely to be misused.
    @property
    def context(self):
        """
        Return the current rendering context [experimental].

        """
        return self._context

    def escape(self, s):
        return self._escape_to_unicode(s)

    def _to_unicode_soft(self, s):
        """
        Convert a basestring to unicode, preserving any unicode subclass.

        """
        # We type-check to avoid "TypeError: decoding Unicode is not supported".
        # We avoid the Python ternary operator for Python 2.4 support.
        if isinstance(s, unicode):
            return s
        return self.unicode(s)

    def _to_unicode_hard(self, s):
        """
        Convert a basestring to a string with type unicode (not subclass).

        """
        return unicode(self._to_unicode_soft(s))

    def _escape_to_unicode(self, s):
        """
        Convert a basestring to unicode (preserving any unicode subclass), and escape it.

        Returns a unicode string (not subclass).

        """
        return unicode(self._escape(self._to_unicode_soft(s)))

    def unicode(self, b, encoding=None):
        """
        Convert a byte string to unicode, using string_encoding and decode_errors.

        Arguments:

          b: a byte string.

          encoding: the name of an encoding.  Defaults to the string_encoding
            attribute for this instance.

        Raises:

          TypeError: Because this method calls Python's built-in unicode()
            function, this method raises the following exception if the
            given string is already unicode:

              TypeError: decoding Unicode is not supported

        """
        if encoding is None:
            encoding = self.string_encoding

        # TODO: Wrap UnicodeDecodeErrors with a message about setting
        # the string_encoding and decode_errors attributes.
        return unicode(b, encoding, self.decode_errors)

    def _make_load_partial(self):
        """
        Return the load_partial function to pass to RenderEngine.__init__().

        """
        if self.partials is None:
            return self.loader.load_name

        # Otherwise, create a load_partial function from the custom partial
        # loader that satisfies RenderEngine requirements (and that provides
        # a nicer exception, etc).
        partials = self.partials

        def load_partial(name):
            template = partials.get(name)

            if template is None:
                raise TemplateNotFoundError("Name %s not found in partials: %s" %
                                            (repr(name), type(partials)))

            # RenderEngine requires that the return value be unicode.
            return self._to_unicode_hard(template)

        return load_partial

    # TODO: add unit tests for this method.
    def load_template(self, template_name):
        """
        Load a template by name from the file system.

        """
        return self.loader.load_name(template_name)

    def _render_string(self, template, *context, **kwargs):
        """
        Render the given template string using the given context.

        """
        # RenderEngine.render() requires that the template string be unicode.
        template = self._to_unicode_hard(template)

        context = ContextStack.create(*context, **kwargs)
        self._context = context

        # We keep this type-check as an added check because this method is
        # called with template strings coming from potentially externally-
        # supplied functions like self.literal, self.load_partial, etc.
        # Beyond this point, we have much better control over the type.
        if type(template) is not unicode:
            raise Exception("Argument 'template' not unicode: %s: %s" % (
                    type(template), repr(template)))

        parsed_template = self.parser.parse(template=template)
        rendered = parsed_template.render(context)

        return unicode(rendered)

    def _render_object(self, obj, *context, **kwargs):
        """
        Render the template associated with the given object.

        """
        loader = self.loader

        # TODO: consider an approach that does not require using an if
        #   block here.  For example, perhaps this class's loader can be
        #   a SpecLoader in all cases, and the SpecLoader instance can
        #   check the object's type.  Or perhaps Loader and SpecLoader
        #   can be refactored to implement the same interface.
        if isinstance(obj, TemplateSpec):
            loader = SpecLoader(loader)
            template = loader.load(obj)
        else:
            template = loader.load_object(obj)

        context = [obj] + list(context)

        return self._render_string(template, *context, **kwargs)

    def render_path(self, template_path, *context, **kwargs):
        """
        Render the template at the given path using the given context.

        Read the render() docstring for more information.

        """
        loader = self.loader
        template = loader.read(template_path)

        return self._render_string(template, *context, **kwargs)

    def render(self, template, *context, **kwargs):
        """
        Render the given template (or template object) using the given context.

        Returns the rendering as a unicode string.

        Prior to rendering, templates of type str are converted to unicode
        using the string_encoding and decode_errors attributes.  See the
        constructor docstring for more information.

        Arguments:

          template: a template string of type unicode or str, or an object
            instance.  If the argument is an object, the function first looks
            for the template associated to the object by calling this class's
            get_associated_template() method.  The rendering process also
            uses the passed object as the first element of the context stack
            when rendering.

          *context: zero or more dictionaries, ContextStack instances, or objects
            with which to populate the initial context stack.  None
            arguments are skipped.  Items in the *context list are added to
            the context stack in order so that later items in the argument
            list take precedence over earlier items.

          **kwargs: additional key-value data to add to the context stack.
            As these arguments appear after all items in the *context list,
            in the case of key conflicts these values take precedence over
            all items in the *context list.

        """
        if isinstance(template, _STRING_TYPES):
            return self._render_string(template, *context, **kwargs)
        # Otherwise, we assume the template is an object.

        return self._render_object(template, *context, **kwargs)
