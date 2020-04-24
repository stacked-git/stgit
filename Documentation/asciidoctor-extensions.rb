require 'asciidoctor'
require 'asciidoctor/extensions'

module StGit
  module Documentation
    class LinkGitProcessor < Asciidoctor::Extensions::InlineMacroProcessor
      use_dsl

      named :chrome

      def process(parent, target, attrs)
        gitscm = 'https://git-scm.com/docs/'
        if parent.document.doctype == 'book' \
          or parent.document.doctype == 'article'
          "<ulink url=\"#{gitscm}#{target}\">" \
          "#{target}(#{attrs[1]})</ulink>"
        elsif parent.document.basebackend? 'html'
          %(<a href="#{gitscm}#{target}">#{target}(#{attrs[1]})</a>)
        elsif parent.document.basebackend? 'docbook'
          "<citerefentry>\n" \
            "<refentrytitle>#{target}</refentrytitle>" \
            "<manvolnum>#{attrs[1]}</manvolnum>\n" \
          "</citerefentry>"
        end
      end
    end

    class LinkManProcessor < Asciidoctor::Extensions::InlineMacroProcessor
      use_dsl

      named :chrome

      def process(parent, target, attrs)
        if parent.document.doctype == 'book' \
          or parent.document.doctype == 'article'
          "<ulink url=\"#{target}.html\">#{target}(#{attrs[1]})</ulink>"
        elsif parent.document.basebackend? 'html'
          %(<a href="#{target}.html">#{target}(#{attrs[1]})</a>)
        elsif parent.document.basebackend? 'docbook'
          "<citerefentry>\n" \
            "<refentrytitle>#{target}</refentrytitle>" \
            "<manvolnum>#{attrs[1]}</manvolnum>\n" \
          "</citerefentry>"
        end
      end
    end

    class LinkStgProcessor < Asciidoctor::Extensions::InlineMacroProcessor
      use_dsl

      named :chrome

      def process(parent, target, attrs)
        if parent.document.doctype == 'book' \
          or parent.document.doctype == 'article'
          "<ulink url=\"stg-#{target}.html\">stg #{target}</ulink>"
        elsif parent.document.basebackend? 'html'
          %(<a href="stg-#{target}.html">stg #{target}</a>)
        elsif parent.document.basebackend? 'docbook'
          "<citerefentry>\n" \
            "<refentrytitle>stg-#{target}</refentrytitle>" \
            "<manvolnum>1</manvolnum>\n" \
          "</citerefentry>"
        end
      end
    end

    class LinkStgSubProcessor < Asciidoctor::Extensions::InlineMacroProcessor
      use_dsl

      named :chrome

      def process(parent, target, attrs)
        if parent.document.doctype == 'book' \
          or parent.document.doctype == 'article'
          "<ulink url=\"stg-#{target}.html\">#{target}</ulink>"
        elsif parent.document.basebackend? 'html'
          %(<a href="stg-#{target}.html">#{target}</a>)
        elsif parent.document.basebackend? 'docbook'
          "<citerefentry>\n" \
            "<refentrytitle>stg-#{target}</refentrytitle>" \
            "<manvolnum>1</manvolnum>\n" \
          "</citerefentry>"
        end
      end
    end

    class DocumentPostProcessor < Asciidoctor::Extensions::Postprocessor
      def process document, output
        if document.basebackend? 'docbook'
          mansource = document.attributes['mansource']
          manversion = document.attributes['manversion']
          manmanual = document.attributes['manmanual']
          new_tags = "" \
            "<refmiscinfo class=\"source\">#{mansource}</refmiscinfo>\n" \
            "<refmiscinfo class=\"version\">#{manversion}</refmiscinfo>\n" \
            "<refmiscinfo class=\"manual\">#{manmanual}</refmiscinfo>\n"
          output = output.sub(/<\/refmeta>/, new_tags + "</refmeta>")
        end
        output
      end
    end
  end
end

Asciidoctor::Extensions.register do
  inline_macro StGit::Documentation::LinkGitProcessor, :linkgit
  inline_macro StGit::Documentation::LinkManProcessor, :linkman
  inline_macro StGit::Documentation::LinkStgProcessor, :linkstg
  inline_macro StGit::Documentation::LinkStgSubProcessor, :linkstgsub
  postprocessor StGit::Documentation::DocumentPostProcessor
end
