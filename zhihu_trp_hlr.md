知乎用户对特朗普和希拉里的看法——基于爬虫和文本挖掘技术的分析
================
Ye Ling

摘要
----

学了R语言后就一直手痒，想从浩瀚的网络中抓一些数据来练练手。最近学习了爬虫终于可以一试身手了。

本文写了一个专爬知乎内容的爬虫，只需输入知乎话题编号（如19552910，对应php语言)，就可以抓取该话题下所有的精华问题，以及每个问题下排名前十的最佳回答，包括回答的全部正文，作者，发布日期，点赞数和评论数。

本文从知乎上分别抓取了讨论美国大选的两位主角特朗普和希拉里的所有精华问答，共有近20000篇，并用文本挖掘技术加以分析。让我们看看知乎用户对特朗普和希拉里是怎么看的吧。有种观点认为知乎对特朗普是一边倒的支持，数据是否支持这种看法呢？

爬虫的编写
----------

本文爬虫主要调用了rvest包，rvest是抓取静态网页的利器，使用起来远比RCurl包便捷。当然缺点是对动态加载的网页就无能为力了。

rvest最主要的三个函数是read\_html()，html\_nodes和html\_text()。功能如下：

read\_html()：传入网址和编码方式（中文网页一般是UTF-8），即可读取整个网页。 html\_nodes()：传入读取的网页和css节点，定位到所有符合要求的节点位置。 html\_text()：传入html\_nodes()的定位结果，提取所有节点下的文本内容，以向量的形式保存。

完整的爬虫程序如下：

``` r
library(rvest)     # 爬虫
library(magrittr)  # 管道运算符

# 输出一个数字，表示该话题的精华有多少页，需要传入话题编号
get_zhihu_page <- function(topic) {  
  paste0("https://www.zhihu.com/topic/", topic, "/top-answers") %>%
    read_html(encoding="UTF-8") %>%
    html_nodes(".zm-invite-pager > span> a:nth-child(1)") %>%
    html_text() %>%
    extract(3) %>%
    as.numeric()
}
#get_zhihu_page(20023724)  # 结果正确
#get_zhihu_page(19674181)

# 获取话题下第n页所有问题的网址
# 第一页和其它页网址不一样，分别处理
page2url <- function(n, topic) {
  if (n==1) {
    qstn <- paste0("https://www.zhihu.com/topic/", topic, "/top-answers") %>%
      read_html(encoding="UTF-8") %>%
      html_nodes("div.feed-item > div:nth-child(6) > div:nth-child(1) > h2:nth-child(1) > a:nth-child(1)") %>%
      html_attr("href") %>%           # 获取全文链接
      extract(grep("/question/", .))  #筛选出有用的链接
  }
  else {
    qstn <- paste0("https://www.zhihu.com/topic/", topic, "/top-answers?page=", n) %>%
      read_html(encoding="UTF-8") %>%
      html_nodes("div.feed-item > div:nth-child(6) > div:nth-child(1) > h2:nth-child(1) > a:nth-child(1)") %>%
      html_attr("href") %>% # 获取全文链接
      extract(grep("/question/", .))    #筛选出有用的链接
  }
  sub("http://www.zhihu.com", "", qstn) %>%
    paste0("https://www.zhihu.com", .)
}


# 该话题下所有精华问题的网址
get_zhihu_qstn <- function(topic) { 
  get_zhihu_page(topic) %>%
    c(1:.) %>%
    sapply(page2url, topic) %>%
    unlist
}
#test <- get_zhihu_qstn(19674181)  # 结果正确
#get_zhihu_qstn(20023724)          # 测试成功

# 从问题的网址获取最热门的十条回答，组成data.frame形式
# css节点的获取：用火狐浏览器打开相应的网页，鼠标停在需要抓取的内容的位置，右键查看元素，即跳至查看器相应位置，
# 在该位置点右键，选复制-> css选择器即可。
# 再复制几个同类型内容的css节点，观察异同，保留相同的部分，删去不同的部分（从冒号开始，包括冒号，一直删到">"前）。
# 这样的节点就对应了这一类内容的所有内容
qstn2ans <- function(url) {
  web <- read_html(url, encoding="UTF-8")
  
  question <- web %>%
    html_nodes("span.zm-editable-content") %>%
    html_text()
  
  author <- web %>%
    html_nodes("div.zm-item-answer > div:nth-child(6) > div:nth-child(1) > span") %>%
    html_text() %>%
    tm::stripWhitespace()
  
  text <- web %>%
    html_nodes("div.zm-item-answer > div:nth-child(7) > div:nth-child(2)") %>% 
    html_text()
  
  zan <- web %>%
    html_nodes("div.zm-item-answer > div:nth-child(5) > button:nth-child(1) > span:nth-child(2)") %>%
    html_text()
  
  date <- web %>%
    html_nodes("div.zm-item-answer > div:nth-child(9) > div:nth-child(1) > a:nth-child(1)") %>%
    html_text()  
  
  commt_count <- web %>%
    html_nodes("div.zm-item-answer > div:nth-child(9) > div:nth-child(1) > a:nth-child(2)") %>%
    html_text() %>%
    tm::stripWhitespace()
  
  data.frame(question=question, author=author, 
             text=text, zan=zan, date=date, 
             commt_count=commt_count, stringsAsFactors = F)
}

#tmp <- qstn2ans("https://www.zhihu.com/question/20594192")

# 用户需要调用的函数
# 传入话题编号，获得所有精华问答
get_zhihu_ans <- function(topic) {   # 输入话题编号
  ans <- get_zhihu_qstn(topic) %>%     # 得到所有问题网址
    lapply(qstn2ans) %>%
    Reduce(rbind, .)
  
  write.csv(ans, paste0("E:/QUANT/text mining/ans_of_", topic, ".csv"))
  return(ans)
}
```

数据清洗
--------

特朗普话题共抓取了10112篇文章，希拉里话题共抓取了9836篇文章。

先来看看抓取的特朗普数据"ans\_of\_trp"和希拉里数据"ans\_of\_hlr"的结构。text字段就是回答的正文，我们主要分析这个字段。

``` r
str(ans_of_trp)
```

    ## Classes 'data.table' and 'data.frame':   10112 obs. of  6 variables:
    ##  $ question   : chr  "得知自己当选的时刻，川普的真实内心究竟是怎样的？（附图）" "得知自己当选的时刻，川普的真实内心究竟是怎样的？（附图）" "得知自己当选的时刻，川普的真实内心究竟是怎样的？（附图）" "得知自己当选的时刻，川普的真实内心究竟是怎样的？（附图）" ...
    ##  $ author     : chr  " 小璐 审计狮， love Butters~ " " 知乎用户 Daddy Trolls Best. " " 知乎用户 UIUC/JD, UMN/POL " " 张鑫 " ...
    ##  $ text       : chr  "\r\n推荐你们观看一下视频：2011 年奥巴马和Seth Meyers在白宫晚宴上毫无忌惮＋尖酸刻薄地对川普极尽轻蔑和嘲讽之能事，川普全程脸色铁"| __truncated__ "\r\nWHEN YOU WIN SO MUCH IT'S JUST ANOTHER DAY IN OFFICE. SO BASED. MUCH BOSS. " "\r\n人呐就都不知道，自己就不可以预料。\r\n" "\r\n第一次当美国总统，怎样才能装作很有经验的样子？在线等，挺急的。不抖机灵，我认为川普最大的感受应该是压力吧。川普完成了政治舞"| __truncated__ ...
    ##  $ zan        : chr  "130" "14" "13" "24" ...
    ##  $ date       : chr  "编辑于 2016-11-11" "发布于 2016-11-11" "发布于 2016-11-11" "编辑于 2016-11-11" ...
    ##  $ commt_count: chr  " 28 条评论" " 2 条评论" " 5 条评论" " 3 条评论" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
str(ans_of_hlr)
```

    ## Classes 'data.table' and 'data.frame':   9836 obs. of  6 variables:
    ##  $ question   : chr  "如何评价推特屏蔽#HillaryForPrison以及#HillaryForPrison的流行？" "如何评价推特屏蔽#HillaryForPrison以及#HillaryForPrison的流行？" "如何评价推特屏蔽#HillaryForPrison以及#HillaryForPrison的流行？" "如何评价推特屏蔽#HillaryForPrison以及#HillaryForPrison的流行？" ...
    ##  $ author     : chr  " vczh 专业造轮子，拉黑抢前排。… " " 广田 Extremely deplorable " " Donald Yang A Common Sense Conservative " " 知乎用户 志张性弛，轻薄无威 " ...
    ##  $ text       : chr  "\r\n说明了这些公司的价值观啊，就是认为只有屁民才需要遵守法律。\r\n" "\r\n闭嘴！我们在讨论言论自由\r\n" "\r\n人家twitter 吧是个私企，人家爱怎么管理自己的平台就怎么管理，你还能咋的\r\n" "\r\n我有朋友就在twitter，实际上往往是社会分裂，造成的两派互相举报。举报多了twitter也不好办。\r\n" ...
    ##  $ zan        : chr  "41" "79" "3" "0" ...
    ##  $ date       : chr  "发布于 2016-11-01" "发布于 2016-11-01" "发布于 2016-11-01" "编辑于 2016-11-10" ...
    ##  $ commt_count: chr  " 3 条评论" " 3 条评论" " 3 条评论" " 添加评论" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

对中文语料的清洗主要用到tm, qdap, Rwordseg, tmcn等几个包。过程包括：

1、自定义停用词库：文本中有很多词出现频繁，但对分析问题没有帮助，如“东西”，“方面”，“使用”等等，需要把这些干扰分析的词删除。虽然tmcn包已经提供了一个中文停用词库stopwordCN()，但还不够，不断地人工添加，这是一个繁琐又不可缺少的步骤。

2、自定义需要提取的词性：segmentCN()函数除了分词外，还提供每个词的词性，我们主要分析名词类词汇。

3、向词典中插入临时词汇：有些词分词词典中没有，又是我们预期会遇到的，如“邮件门”，这就需要手动插入。

4、语料清洗，包括去除数字，英文字符，标点符号，多余的空字符，分词，筛选出需要提取的词性，删除单个字的词，删除停用词。

5、最后生成Term Document Matrix(tdm)。

所用到的函数如下：

``` r
library(Rwordseg)    # 中文分词
library(tm)          # 各种text mining函数
library(tmcn)        # 含有中文停用词库stopwordsCN()
library(wordcloud)   # 各种词云
library(wordcloud2)  # 效果更好的词云
library(qdap)        # 各种text mining函数
library(stringr)     # 字符处理
library(plotrix)     # 金字塔图
library(dendextend)  # 美化层次分类图
library(RWeka)       # 生成tokenizer
library(RColorBrewer)# 调色板
library(gridExtra)   # 多张图片排版
library(ggplot2)     # 作图

### 生成corpus前先对语料做预处理

# 自定义停用词库
myStopwords <-  c(stopwordsCN(), "东西", "不会", "方面", "使用",
                 "需要", "没有", "觉得", "知道", "进行", "得到",
                 "应该", "出来", "部分", "用来", "实现", "适合",
                 "大的", "个人", "提供", "可能", "支持", "完成",
                 "感觉", "能够", "看到", "起来", "不能", "希望", 
                 "实际", "利用", "回答", "包括", "例子", "最好",
                 "事情", "不用", "方向", "语言", "一行", "一部分",
                 "一回事", "什么的", "作者", "一句话", "不仅仅是",
                 "意思", "并不是", "另一个", "大多数人", "类似的",
                 "特别是", "越来越多", "不一定", "不可能", "有没有",
                 "特别是", "会不会", "有一天", "有一些", "这就是",
                 "不得不", "看起来", "取决于", "也就是", "能不能",
                 "相当于", "希拉里", "特朗普", "民主党", "共和党",
                 "奥巴马", "这些人", "候选人", "唐纳德", "尤其是",
                 "到时候", "意识到", "每个人")         

# 自定义需要的词性，乱码表示搜狗标准词库
myNature <- c("n","vn","ns","鎼滅嫍鏍囧噯璇嶅簱", "userDefine")   

# 插入临时词汇
ins_words <- c("特朗普", "川普", "川粉", "川黑", "希粉", "希黑",
               "希拉里", "维基解密", "阿桑奇", "克林顿", "奥巴马",
               "政治正确", "白左", "伊万卡", "脱欧", "邮件门")
insertWords(ins_words, save = T)

# 生成tdm需要的分词器，限定长度为1
tokenizer <- function(x){
  RWeka::NGramTokenizer(x, control = Weka_control(min=1, max=1))
}

# 将原始语料转化为向量，每个回答为一个元素
cre_vector <- function(data) {
  data %<>% extract(, "text") %>%
    str_replace_all("[A-Za-z0-9]", "") %>%
    removePunctuation() %>%
    stripWhitespace() %>%          
    segmentCN(nature=T) %>%                       # 分词，并保留词性
    lapply(function(vec) {                        
      vec %<>% extract(names(.) %in% myNature)
    }) %>%                                        # 筛选名词和词组
    lapply(function(vec) {                        
      vec %<>% extract(nchar(.) >1)               # 删去单个字的词汇
    }) %>%
    lapply(function(vec) {
      vec %<>% removeWords(myStopwords)
    }) %>%
    lapply(function(vec) {
      vec %<>% paste(collapse=" ")
    }) %>%                # 把所有的词重新粘在一起，以空格分割，形式上类似英文文本                        
    unlist()
}

# 从vec生成tdm
vec2tdm <- function(vec) {
  vec %>% VectorSource() %>%
    VCorpus(list(language= NA)) %>%  # 因为语料是中英文混杂的，所以language=NA
    TermDocumentMatrix(list(tokenize=tokenizer))
}
```

``` r
trp_vector <- cre_vector(ans_of_trp)  # 生成向量，每篇文章为一个元素，文章中词语词之间用空格分割，形式上与英文文本相同
trp_tdm <- vec2tdm(trp_vector)        # 生成TermDocumentMatrix
hlr_vector <- cre_vector(ans_of_hlr)  # trp代表特朗普，hlr代表希拉里
hlr_tdm <- vec2tdm(hlr_vector)
```

文本挖掘需要的一些自定义函数
----------------------------

包括生成词频表，生成层次聚类图和相关词图，代码如下：

``` r
# 从tdm生成top n词频表
tdm2freq <- function(tdm, k=100) {
  tdm %>% as.matrix() %>%
    rowSums() %>%
    sort(decreasing=T) %>%             # 注意不能用order，names会丢失
  extract(1:k) %>%
    data.table(word=names(.), freq=.)
}

## 从tdm生成dendrogram
# sparse 最大允许稀疏度，k 分类框的数目，labels 哪些词标记为红色
tdm2dend <- function(tdm, sparse=0.9, k=2, labels=c(), ...) {
  tdm %<>%
    removeSparseTerms(sparse=sparse) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    branches_attr_by_labels(labels=labels, "red") %>%
    {
      plot(.)
      rect.dendrogram(.,k=k, border="grey50")
    }
}

## 从tdm生成某个词的相关词图
 # term 需要查询的词汇，r 最低相关系数
tdm2ass_gram <- function(tdm, term, r, name="") {
  tdm %>%
    findAssocs(term, r) %>%
    list_vect2df() %>%
    extract(,2:3) %>%
    ggplot(aes(x=.[,2],y=.[, 1])) +
    geom_point(size=3) +
    theme_light() +
    xlab("相关系数") +
    ylab(paste0("与", term, "相关的词")) +
    ggtitle(paste0(name, "的“", term, "”"))
}
```

高频词和词云
------------

下面进入文本挖掘阶段。来看看知乎用户谈论特朗普时最常出现哪些词吧：

    ## 谈论特朗普时的前十大高频词汇:
    ##  支持者 政治正确 中国人 墨西哥 俄罗斯 正确的 伊斯兰 华尔街 全球化 华盛顿

![Alt text](https://github.com/wellingtonyl/zhihu/master/images/trump_wordcloud.png)

再来看看希拉里的：

    ## 谈论希拉里时的前十大高频词汇:
    ##  支持者 犹太人 基金会 政治正确 邮件门 中国人 俄罗斯 维基解密 国务卿 阿桑奇

需要说明的是，为了避免影响分析，一些超高频词如“特朗普”，“希拉里”，“民主党”，“共和党”，“奥巴马”等已经事先删除了，详见上面的停用词表。

2016年是美国大选之年，所以“支持者”一词出现在两人榜单的第一位不足为奇。

接下来特朗普的榜单上出现了“政治正确”，“中国人”，“墨西哥”，“俄罗斯”，“伊斯兰”，“全球化”等词。一个反对政治正确，叫嚷着要收中国高额关税，要在墨西哥边境造墙，反对伊斯兰移民，反对全球化的特朗普形象跃然纸上。

反观希拉里方面，榜单上多是“犹太人”，“基金会”，“邮件门”，“维基解密”，“阿桑奇”等词汇。一个被克林顿基金会和邮件门丑闻缠身的希拉里形象通过这些词出现在了我们面前。

可见，知乎用户在讨论特朗普时，大多在讨论他的政策取向，而不是花边新闻或丑闻。而对希拉里则以议论各种丑闻为主。知乎用户对两人的倾向可见一斑。

下面是两人共有的高频词汇的词云，“支持者”，“政治正确”，“中国人”，“俄罗斯”等词位居前列，这反应了知乎用户在美国大选期间关注的焦点话题。

金字塔图
--------

金字塔图取了25个两人共有高频词，比较其词频的差异。从图中可以看出在大多数热门词汇上，特朗普方面的讨论热度要远高于希拉里方面，如对“政治正确”，“墨西哥”，“俄罗斯”和“中国人”等话题。只有在“基金会”，“犹太人”等希拉里“专属词汇”上，希拉里才超过了特朗普。总体上，特朗普在知乎上的人气高于希拉里。

相关词图
--------

相关词图描述了一个特定的词与其它词的相关性。可以用相关词图从大量文本中挖掘潜藏的信息。

我们看到当谈到希拉里的“价值观”时，最相关的词是“多元化”。而特朗普的“价值观”则是“民族融合”，“工薪阶层”和“个人主义”。相关词图一下子为我们找出了两人价值观的差异。

情感分析
--------

本文的情感分析使用的是基于情感词典的方法。词典来自于台湾大学研发的中文情感极性词典NTUSD，包括了正面情感词汇库和负面情感词汇库。我们的对每篇文章的情感打分算法是：对文章中的每一个词，如果属于正面情感词汇，这加一分，如果属于负面情感词汇，则减一分，其余词汇记为0，所有词汇评分相加即得到这篇文章的情感得分。

可以看到，特朗普话题的平均情感得分为6.318分，高于希拉里的5.314分。经t检验，p值为0.00002，两人的得分差异是显著的。

所有情感得分为正数的文章中，特朗普的平均得分9.458也高于希拉里的7.936。经t检验，差异也是显著的。

值得一提的是，在所有情感得分为负数的文章中，特朗普的平均得分-2.517，低于希拉里的-1.891（t检验显著）。关于特朗普的负面情感文章总数为1065篇，占比10.5%；希拉里的负面情感文章总数为906篇，占比9.2%。无论是平均得分，负面文章总数还是负面文章占比，特朗普都输给了希拉里。这说明知乎用户对特朗普的评价相比希拉里而言是两级分化的，喜欢他的人自是喜欢，讨厌他的人则极为讨厌他。

情感分析再次向我们证明了知乎用户总体上喜欢特朗普更甚于希拉里，同时对特朗普的情感趋向于两极分化。

总结
----

本文首先编写了一个用于抓取知乎内容的爬虫，可以抓取任意给定主题下的所有精华问答。用此爬虫程序获取了知乎话题“特朗普”和“希拉里”共近20000篇问答。在此基础上进行了文本挖掘。

本文用数据证明了知乎用户对特朗普乐意讨论其反政治正确的种种作为，以及其对中国，墨西哥和俄罗斯的政策；对希拉里，则热衷于讨论她的邮件门丑闻和克林顿基金会的丑闻。人气方面，特朗普远高于希拉里。

通过文本挖掘，发现特朗普的价值观以“民族融合”，“工薪阶层”和“个人主义”为主，希拉里的价值观则以“多元化”为主。

通过情感分析，数据证明知乎用户总体上更喜欢特朗普，验证了本文一开始的设想。而在双方的反对者方面，特朗普的反对者反对情绪更为激烈一些。也就是说，知乎用户对特朗普的情感趋向于两极分化。
