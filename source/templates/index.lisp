(in-package :litterae)

;; Enable lsx syntax in compile time
(eval-when (:compile-toplevel)
  (lsx:enable-lsx-syntax))

(defun has-attr? (attr)
  "Return true if the attribute `attr' holds a value, false otherwise."
  (not-empty? (funcall attr)))

(lsx:deftag seo-head (&key title
                           (description nil)
                           (url nil))
  <head>
    <meta charset="utf-8" />
    <title>{(concatenate 'string (funcall title) " |")} Documentation</title>
    {(and (has-attr? description) <meta name="description" content={description} />)}
    
    <meta name="og:title" property="og:title" content={(concat-as-string (funcall title)
                                                                         " | Documentation")} />
    <meta property="og:type" content="website" />
    {(and (has-attr? description) <meta name="og:description" content={description} />)}
    
    {(and (has-attr? url) <link href={url} rel="canonical" />)}
    </head>

    :documentation "Holds the structure for a good performing SEO head.")

(lsx:deftemplate index-template ()
  ((title :initform nil)
   (description :initform nil)
   (url :initform nil)
   (body :initform nil)
   (lang :initform "en"))
  
  (:render
<html lang={lang}>
  <seo-head title={title} description={description} url={url} />
  <body>
    {body}
  </body>
</html>))

