(in-package :litterae)

;; Enable lsx syntax in compile time
(eval-when (:compile-toplevel)
  (lsx:enable-lsx-syntax))

(defun has-attr? (attr)
  "Return true if the attribute `attr' holds a value, false otherwise."
  (not (str:empty? (funcall attr))))

(lsx:deftag seo-head (&key title
                           (description nil)
                           (url nil))
  <head>
    <meta charset="utf-8" />
    <title>{(str:concat (funcall title) " |")} Documentation</title>
    {(and (has-attr? description) <meta name="description" content={description} />)}
    
    <meta name="og:title" property="og:title" content={(str:concat (funcall title)
                                                                         " | Documentation")} />
    <meta property="og:type" content="website" />
    {(and (has-attr? description) <meta name="og:description" content={description} />)}
    
    {(and (has-attr? url) <link href={url} rel="canonical" />)}

    <link rel="stylesheet" href="//fonts.googleapis.com/css?family=Lato:400,400italic,900,900italic" />
    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.css" />
    <link rel="stylesheet" href="./styles.css" />
    <link rel="stylesheet" href="./highlight-theme-lovelace.css" />
    </head>)

(lsx:deftag top-menu (&key title)
  <div class="top-menu">
    <a href="#">{title}</a>
    <input type="text" placeholder="Search..." />
  </div>)

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
    <top-menu title={title} />
    {body}
  </body>
</html>))

