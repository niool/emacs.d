(require 'newsticker)

;; W3M HTML renderer isn't essential, but it's pretty useful.
;; (require 'w3m)
;; (setq newsticker-html-renderer 'w3m-region)

;; We want our feeds pulled every 10 minutes.
(setq newsticker-retrieval-interval 600)

;; Setup the feeds. We'll have a look at these in just a second.
;; (LABEL URL &START-TIME INTERVAL WGET-ARGUMENTS)
(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list
      (quote
       (("dyddqczx第一电动汽车资讯" "http://www.diyiev.com/feed/rss.php?mid=21" nil nil nil)
        ("lxshsys理想生活实验室" "http://www.toodaylab.com/feed" nil nil nil)
        ("GeekCar" "http://www.geekcar.net/feed" nil nil nil)
        ("EngadgetChina" "https://cn.engadget.com/rss.xml" nil nil nil)
        ("jmxw界面新闻" "https://a.jiemian.com/index.php?m=article&a=rss" nil nil nil))
       )
)

;; Optionally bind a shortcut for your new RSS reader.
(global-set-key (kbd "C-c r") 'newsticker-treeview)

;; Don't forget to start it!
(newsticker-start)
