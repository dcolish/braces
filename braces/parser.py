# coding: utf-8

"""
Provides a class for parsing template strings.

This module is only meant for internal use by the renderengine module.

"""

#TODO:dc: Reimplement in C

import re

from braces.common import TemplateNotFoundError
from braces.parsed import ParsedTemplate


DEFAULT_DELIMITERS = (u'{{', u'}}')
END_OF_LINE_CHARACTERS = [u'\r', u'\n']
NON_BLANK_RE = re.compile(ur'^(.)', re.M)


def _compile_template_re(delimiters=None):
    """
    Return a regular expresssion object (re.RegexObject) instance.

    """
    if delimiters is None:
        delimiters = DEFAULT_DELIMITERS

    # The possible tag type characters following the opening tag,
    # excluding "=" and "{".
    tag_types = "!>&/#^"

    # TODO: are we following this in the spec?
    #
    #   The tag's content MUST be a non-whitespace character sequence
    #   NOT containing the current closing delimiter.
    #
    # otag is opening delimiter
    # ctag is closing delimiter
    tag = r"""
        (?P<whitespace>[\ \t]*)
        %(otag)s \s*
        (?:
          (?P<change>=) \s* (?P<delims>.+?)   \s* = |
          (?P<raw>{)    \s* (?P<raw_name>.+?) \s* } |
          (?P<tag>[%(tag_types)s]?)  \s* (?P<tag_key>[\s\S]+?)
        )
        \s* %(ctag)s
    """ % {'tag_types': tag_types,
           'otag': re.escape(delimiters[0]),
           'ctag': re.escape(delimiters[1])}

    return re.compile(tag, re.VERBOSE)


class ParsingError(Exception):

    pass


class Parser(object):

    _delimiters = None
    _template_re = None

    def __init__(self, renderer, delimiters=None):
        """
        Construct an instance.

        Arguments:

          renderer: a Renderer instance.

        """
        if delimiters is None:
            delimiters = DEFAULT_DELIMITERS

        self.renderer = renderer
        self.compile_template_re(delimiters)

    def compile_template_re(self, delimiters):
        self._delimiters = delimiters
        self._template_re = _compile_template_re(delimiters)

    def parse(self, template, start_index=0, section_key=None):
        """
        Parse a template string starting at some index.

        This method uses the current tag delimiter.

        Arguments:

          template: a unicode string that is the template to parse.

          index: the index at which to start parsing.

        Returns:

          a ParsedTemplate instance.

        """
        parse_tree = []
        index = start_index
        template_length = len(template)
        while True:
            match = self._template_re.search(template, index)

            if match is None:
                break

            match_index = match.start()
            end_index = match.end()

            # All text before the first delimiter match
            before_tag = template[index : match_index]

            parse_tree.append(before_tag)

            matches = match.groupdict()

            # Normalize the matches dictionary.
            if matches['change']:
                matches.update(tag='=', tag_key=matches['delims'])
            elif matches['raw']:
                matches.update(tag='&', tag_key=matches['raw_name'])

            tag_type = matches['tag']
            tag_key = matches['tag_key']
            leading_whitespace = matches['whitespace']

            # Standalone (non-interpolation) tags consume the entire line,
            # both leading whitespace and trailing newline.
            did_tag_begin_line = did_tag_end_line = False
            if (match_index == 0 or
                template[match_index - 1] in END_OF_LINE_CHARACTERS):
                did_tag_begin_line = True

            if (end_index == template_length or
                template[end_index] in END_OF_LINE_CHARACTERS):
                did_tag_end_line = True

            is_tag_interpolating = tag_type in ['', '&']

            if (did_tag_begin_line and did_tag_end_line and
                not is_tag_interpolating):
                if (end_index < template_length and
                    template[end_index] in END_OF_LINE_CHARACTERS):
                    end_index += 1
            elif leading_whitespace:
                parse_tree.append(leading_whitespace)
                match_index += len(leading_whitespace)
                leading_whitespace = ''

            if tag_type == '/':
                if tag_key != section_key:
                    raise ParsingError("Section end tag mismatch: %s != %s" % (
                            tag_key, section_key))

                return ParsedTemplate(parse_tree), match_index, end_index

            index = self._handle_tag_type(template, parse_tree,
                                          tag_type, tag_key,
                                          leading_whitespace, end_index)

        # Save the rest of the template.
        parse_tree.append(template[index:])

        return ParsedTemplate(parse_tree)

    def _parse_section(self, template, start_index, section_key):
        """
        Parse the contents of a template section.

        Arguments:

          template: a unicode template string.

          start_index: the string index at which the section contents begin.

          section_key: the tag key of the section.

        Returns: a 3-tuple:

          parsed_section: the section contents parsed as a ParsedTemplate
            instance.

          content_end_index: the string index after the section contents.

          end_index: the string index after the closing section tag (and
            including any trailing newlines).

        """
        parsed_section, content_end_index, end_index = (
            self.parse(template=template,
                       start_index=start_index,
                       section_key=section_key)
            )

        return (parsed_section, template[start_index:content_end_index],
                end_index)

    def _make_get_section(self, name, parsed_template_, template_, delims):
        def get_section(context):
            """
            Returns: a string of type unicode.

            """
            template = template_
            parsed_template = parsed_template_
            data = context.get(name)

            # From the spec:
            #
            #   If the data is not of a list type, it is coerced into a list
            #   as follows: if the data is truthy (e.g. `!!data == true`),
            #   use a single-element list containing the data, otherwise use
            #   an empty list.
            #
            if not data:
                data = []
            else:
                # The least brittle way to determine whether something
                # supports iteration is by trying to call iter() on it:
                #
                #   http://docs.python.org/library/functions.html#iter
                #
                # It is not sufficient, for example, to check whether the item
                # implements __iter__ () (the iteration protocol).  There is
                # also __getitem__() (the sequence protocol).  In Python 2,
                # strings do not implement __iter__(), but in Python 3 they do.
                try:
                    iter(data)
                except TypeError:
                    # Then the value does not support iteration.
                    data = [data]
                else:
                    if isinstance(data, (basestring, dict)):
                        # Do not treat strings and dicts (which are iterable) as lists.
                        data = [data]
                    # Otherwise, treat the value as a list.

            parts = []
            for element in data:
                if callable(element):
                    # Lambdas special case section rendering and bypass pushing
                    # the data value onto the context stack.  From the spec--
                    #
                    #   When used as the data value for a Section tag, the
                    #   lambda MUST be treatable as an arity 1 function, and
                    #   invoked as such (passing a String containing the
                    #   unprocessed section contents).  The returned value
                    #   MUST be rendered against the current delimiters, then
                    #   interpolated in place of the section.
                    #
                    #  Also see--
                    #
                    #   https://github.com/defunkt/pystache/issues/113
                    #
                    # TODO: should we check the arity?
                    new_template = element(template)
                    new_parsed_template = self.parse(new_template)
                    parts.append(new_parsed_template.render(context))
                    continue

                context.push(element)
                parts.append(parsed_template.render(context))
                context.pop()

            return unicode(''.join(parts))

        return get_section

    # TODO: rename context to stack throughout this module.
    def _get_string_value(self, context, tag_name):
        """
        Get a value from the given context as a basestring instance.

        """
        val = context.get(tag_name)

        if callable(val):
            # According to the spec:
            #
            #     When used as the data value for an Interpolation tag,
            #     the lambda MUST be treatable as an arity 0 function,
            #     and invoked as such.  The returned value MUST be
            #     rendered against the default delimiters, then
            #     interpolated in place of the lambda.
            template = val()
            if not isinstance(template, basestring):
                # In case the template is an integer, for example.
                template = str(template)
            if type(template) is not unicode:
                template = self.literal(template)
            val = self._render(template, context)

        if not isinstance(val, basestring):
            val = str(val)

        return val

    def _make_get_literal(self, name):
        def get_literal(context):
            """
            Returns: a string of type unicode.

            """
            s = self._get_string_value(context, name)
            s = self.renderer.literal(s)
            return s

        return get_literal

    def _make_get_escaped(self, name):

        def get_escaped(context):
            """
            Returns: a string of type unicode.

            """
            s = self._get_string_value(context, name)
            s = self.renderer.escape(s)
            return s

        return get_escaped

    def _make_get_partial(self, template):
        def get_partial(context):
            """
            Returns: a string of type unicode.

            """
            # TODO: the parsing should be done before calling this function.
            parsed_template = self.parse(template)
            return parsed_template.render(context)

        return get_partial

    def _make_get_inverse(self, name, parsed_template):
        def get_inverse(context):
            """
            Returns a string with type unicode.

            """
            # TODO: is there a bug because we are not using the same
            #   logic as in _get_string_value()?
            data = context.get(name)
            # Per the spec, lambdas in inverted sections are considered truthy.
            if data:
                return u''
            return parsed_template.render(context)

        return get_inverse

    def _handle_tag_type(self, template, parse_tree, tag_type, tag_key,
                         leading_whitespace, end_index):
        renderer = self.renderer

        def _handle_change(end_index):
            delimiters = tag_key.split()
            self.compile_template_re(delimiters)
            return end_index, None

        def _handle_hash(end_index):
            parsed_section, section_contents, end_index = self._parse_section(
                template, end_index, tag_key)
            return end_index, self._make_get_section(
                tag_key, parsed_section, section_contents, self._delimiters)

        def _handle_caret(end_index):
            parsed_section, section_contents, end_index = self._parse_section(
                template, end_index, tag_key)
            return end_index, self._make_get_inverse(tag_key, parsed_section)

        def _handle_gt(end_index):
            try:
                template = renderer.load_partial(tag_key)
            except TemplateNotFoundError:
                template = u''

            # Indent before rendering.
            template = re.sub(
                NON_BLANK_RE, leading_whitespace + ur'\1', template)

            return end_index, self._make_get_partial(template)

        dispatch = {
            '!': lambda x: (x, None),
            '=': _handle_change,
            '': lambda x: (end_index, self._make_get_escaped(tag_key)),
            '&': lambda x: (end_index, self._make_get_literal(tag_key)),
            '#': _handle_hash,
            '^': _handle_caret,
            '>': _handle_gt,
            }

        func = None
        try:
            end_index, func = dispatch[tag_type](end_index)
        except KeyError:
            raise Exception("Unrecognized tag type: %s" % repr(tag_type))

        if func is None:
            return end_index
        parse_tree.append(func)

        return end_index
