## Make Slides Using R
## Teammates: Jinfei Xue, Guangyan Yu, Yaotang Luo, Shiyu Zhang

library(officer)
library(magrittr)
library(tidyverse)
library(readxl)

##  The temp
pres1 <- read_pptx("drop1.pptx") 


##  get the layout
layout_summary(pres1)
master <- "Droplet"


## Slide 12
layout_properties(x = pres1, layout = "Title Slide", master = master )
pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Advantages of a Bear Market") %>% 
  ph_with_ul(type = "subTitle",
             str_list = c("Yes there is a positive side to",
                          "a Bear Market! "),
             level_list = c(1,1))
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout = "Title and Content", master=master) %>%
  ph_with_text(str="Investing in Stocks",type="title") %>%
  ph_with_ul(type="body", index=2, str_list=c("1. Represents ownership in a firm",
                                              "2. Earn a return in two ways",
                                              "3. Stockholders have claim on all assets",
                                              "4. Right to vote for directors and on certain issues",
                                              "5. Two types"),
             level_list=c(1,1,1,1,1))

## Slide 34
layout_properties(x = pres1, layout = "Picture with Caption", master = master )
pres1 %<>% add_slide(layout = "Picture with Caption", master = master ) %>% 
  ph_with_text(type = "title", str = "Investing in Stocks: Sample Corporate Stock Certificate") %>% 
  ph_with_img_at( src = "3.jpg",left=4, height = 4,rot=0,width = 5,top=3)
pres1 %<>% add_slide(layout = "Picture with Caption", master = master ) %>% 
  ph_with_text(type = "title", str = "What is a Bear Market?") %>% 
  ph_with_text(type="body",index=2, str="A decline of 15-20% of the broad market coupled with pessimistic sentiment underlying the stock market.")%>%
  ph_with_img(type = "pic", src = "4.jpg", width = 5, height = 4)

## Slide 56
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout = "Title and Content", master = master ) %>% 
  ph_with_text(type = "title", str = "Stock Market Indexes: the Dow Jones Industrial Average") %>% 
  ph_with_img_at(src = "5.jpg", height = 5, width = 9, left = 2, top = 2)
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Dow Jones") %>%
  ph_with_img_at(src = "6.jpg", height = 5, width = 9, left = 2, top = 2) 

## Slide 78
layout_properties(x = pres1, layout = "Title Slide", master = master )
pres1 %<>% add_slide(layout = "Title and Content", master=master) %>%
  ph_with_text(str="The last Bear market",type="title") %>%
  ph_with_ul(type="body", index=2, str_list=c("Sept. 30, 2002  Dow 7528",
                                              "Jan. 5, 2004  Dow 10,568",
                                              "Oct. 8, 2007  Dow 14093"),
             level_list=c(1,1,1))
pres1 %<>% add_slide(layout = "Title and Content", master=master) %>%
  ph_with_text(str="What do I do in a Bear Market",type="title") %>%
  ph_with_ul(type="body", index=2, str_list=c("Decide whether this is a market correction or the start of something more",
                                              "Review the stocks you own",
                                              "Review stocks you wanted to own but were too expensive at time of research",
                                              "Check your portfolio for balance or the type of stocks you own"),
             level_list=c(1,1,1,1))

## remove the template slide
pres1 <- remove_slide(pres1, index = 1)

print(pres1, target = "Slides_Example.pptx") 
