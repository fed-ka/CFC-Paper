library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(tidyr)
library(hrbrthemes)
library(gridExtra)

## -------- Prep/Combine Data Sets ------------

# import data into data frames
setwd("~/GitHub/S&C/Final/Data")
books.df<-read.csv("books-1-2.csv",head=T)
creators.df<-read.csv("creator_1.2_merged_gender_clean.csv", head=T)
events.df<-read.csv("events.csv",head=T)
members.df<-read.csv("members.csv",head=T)

# filter events by "borrow," then double merge with both S&C exports
borrow.df <- events.df[events.df$event_type == "Borrow",]
member.borrow.df <- merge(borrow.df, members.df, by.x = "member_names", by.y = "name")
member.creator.borrow.df <- merge(member.borrow.df, creators.df, by.x = "item_authors", by.y = "sort.name")
merged.df <- member.creator.borrow.df[, c("item_title",	"item_year", "name", "Gender","member_names",	"member_sort_names",	"member_uris",		"gender",
                                          "nationalities",	"birth_year",	"death_year",	"membership_years")]
colnames(merged.df) <-  c("item_title",	"item_year",	"creator_name", "creator_gender",	"member_names",	"member_sort_names",	"member_uris",		"member_gender",
                          "member_nationalities",	"member_birth",	"member_death",	"membership_years")

# circulation by gender. c = circulation; a = all, r= reading; 

c.a.r.m <- filter(merged.df, creator_gender == "Male")
c.a.r.w <- filter(merged.df, creator_gender == "Female")
c.m.r.m <- filter(merged.df, member_gender == "Male" & creator_gender == "Male")
c.m.r.w <- filter(merged.df, member_gender == "Male" & creator_gender == "Female")
c.w.r.m <- filter(merged.df, member_gender == "Female" & creator_gender == "Male")
c.w.r.w <- filter(merged.df, member_gender == "Female" & creator_gender == "Female")
c.m.r.a <- filter(merged.df, member_gender == "Male")
c.w.r.a <- filter(merged.df, member_gender == "Female")


## ----------- Figure 1 and 2 -----------
members.gender.totals <- as.data.frame(table(members.df$gender), stringsAsFactors = F)
members.gender.totals[1,1] <- c("Unidentified")
colnames(members.gender.totals) <- c("gender", "member.total")
combined.df <- members.gender.totals
combined.df$borrow.totals <- table(merged.df$member_gender)
# remove unidentified, nonbinary
combined.df = combined.df[-1,]
combined.df = combined.df[-3,]
# define custom colors
sc_colors = c("#58D2C8", "#FD9D21")

fig.1 <- ggplot(combined.df, aes(x = "", y = member.total, fill = gender)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = sc_colors) +
  labs(x = "NULL",
       y = NULL,
       ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")


fig.2 <- ggplot(combined.df, aes(x = "", y = borrow.totals, fill = gender)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = sc_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")

fig.2


## ------------  Figure 3 and Figure 4 -------------
# books by gender total

books.creator.df <- merge(books.df, creators.df, by.x = "author", by.y = "sort.name")
books.gender.total <- as.data.frame(table(books.creator.df$Gender))
colnames(books.gender.total) <- c("gender", "all.years")
combined.books <- books.gender.total
books.creator.1919<- books.creator.df[books.creator.df[["year"]] >= "1919",]
books.gender.1919 <- as.data.frame(table(books.creator.1919$Gender))
combined.books$post.1919 <- books.gender.1919$Freq
# remove unidentified
combined.books = combined.books[-1,]
sc_colors = c("#58D2C8", "#FD9D21")

# books total raw: w = 924, m= 4055
# books post 1919 raw: w = 868 m = 2664

fig.3 <- ggplot(combined.books, aes(x = "", y = all.years, fill = gender)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = sc_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")

fig.3

fig.4 <- ggplot(combined.books, aes(x = "", y = post.1919, fill = gender)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = sc_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")

fig.4 

grid.arrange(fig.3, fig.4, ncol=2)

# bestsellers

p.w.men <- filter(pw.bestsell, pw.bestsell$gender == "Male")
p.w.women <- filter(pw.bestsell, pw.bestsell$gender == "Female")

men.decades <- as.data.frame(table(p.w.men$decade))
women.decades <- as.data.frame(table(p.w.women$decade))

decades <- merge(men.decades, women.decades, by.x = Var1)

m.r.w.auth <- as.data.frame(table(m.r.w$creator_name))
w.r.m.auth <- as.data.frame(table(w.r.m$creator_name))
w.r.w.auth <- as.data.frame(table(w.r.w$creator_name))

#PUL

books.gender.pul <- as.data.frame(table(pul.books.df$Gender))

## ------ Figure 5 and Figure 6 ------


# ggplot women reading men/women
# raw: 
dmf <- data.frame(
  gender=c("Men", "Women"),
  count=c(9225, 3864))


ggplot(dmf, aes(x="", count, fill=gender)) +
  geom_col() + 
  coord_flip() +
  scale_fill_brewer(palette="Set3") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")

# ggplot men reading men/women
dmm <- data.frame(
  gender=c("Men", "Women"),
  count=c(5132, 915))

ggplot(dmm, aes(x="", count, fill=gender)) +
  geom_col() + 
  coord_flip() +
  scale_fill_brewer(palette="Set3") +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(size=14, face="bold")) +
  coord_polar(theta = "y")


## -------- Figure 7 --------

## counterfactual lollipop charts

# this data was reimported
canons <- data.frame(read.csv("canons.csv"), headers = T)

ggplot(data = canons, aes(x=author, y=delta)) +
  geom_segment(aes(x=author, xend=author, y=0, yend=delta), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())

canons %>%
  arrange(delta) %>%    
  mutate(author=factor(author, levels=author)) %>%  
  ggplot(aes(x=author, y=delta)) +
  geom_segment(aes(xend=author, yend=0), color="skyblue") +
  geom_point( size=3, color="orange", alpha=0.6) +
  theme_light() +
  coord_flip() +
  labs(
    #y= paste('preferred by men', sprintf('\u2190'), "                                ", sprintf('\u2192'),  'preferred by women'),
    y = "",
    x = "",
  ) +
  theme(
    axis.text.y =element_text(size=10),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())


## -------- Counterfactual Canons Code ------------ 

# Counterfactual canons code! Most popular authors/ Books by gender. This data was exported and combined by hand.

# Step 1. Raw frequency tables of most popular creators.books by group
m.r.m.auth <- as.data.frame(table(c.m.r.m$creator_name))
m.r.w.auth <- as.data.frame(table(c.m.r.w$creator_name))
w.r.m.auth <- as.data.frame(table(c.w.r.m$creator_name))
w.r.w.auth <- as.data.frame(table(c.w.r.w$creator_name))
a.r.w.auth <- as.data.frame(table(c.a.r.w$creator_name))
a.r.m.auth <- as.data.frame(table(c.a.r.m$creator_name))

write.csv(m.r.m.auth, "m_r_m_auth.csv")
write.csv(m.r.w.auth, "m_r_w_auth.csv")
write.csv(w.r.m.auth, "w_r_m_auth.csv")
write.csv(w.r.w.auth, "w_r_w_auth.csv")

m.r.m.book <- as.data.frame(table(paste(c.m.r.m$creator_name,c.m.r.m$item_title)))
m.r.w.book <- as.data.frame(table(paste(c.m.r.w$creator_name,c.m.r.w$item_title)))
w.r.m.book <- as.data.frame(table(paste(c.w.r.m$creator_name,c.w.r.m$item_title)))
w.r.w.book <- as.data.frame(table(paste(c.w.r.w$creator_name,c.w.r.w$item_title)))

write.csv(m.r.m.book, "book_m_r_m.csv")
write.csv(m.r.w.book, "book_m_r_w.csv")
write.csv(w.r.m.book, "book_w_r_m.csv")
write.csv(w.r.w.book, "book_w_r_w.csv")

# Step 2. See what each Group prefers by scaling values 

r.m.diff <- merge(w.r.m.auth, m.r.m.auth, by = "Var1", all = TRUE)
r.m.diff[is.na(r.m.diff)] <- 0
names(r.m.diff)[1] = "auth"
names(r.m.diff)[2] = "women.count"
names(r.m.diff)[3] = "men.count"
r.m.diff[4] <- r.m.diff$women - r.m.diff$men
names(r.m.diff)[4] = "raw.diff"
r.m.diff[5] <- r.m.diff$women.count / sum(r.m.diff$women.count) * 100
names(r.m.diff)[5] = "women.perc"
r.m.diff[6] <- r.m.diff$men.count / sum(r.m.diff$men.count) * 100
names(r.m.diff)[6] = "men.perc"
r.m.diff[7] <- r.m.diff$women.perc - r.m.diff$men.perc
names(r.m.diff)[7] = "perc.diff"

write.csv(r.m.diff, "Male_Auth_Diff.csv")


# freq table differences for female authors

r.w.diff <- merge(w.r.w.auth, m.r.w.auth, by = "Var1", all = TRUE)
r.w.diff[is.na(r.w.diff)] <- 0
names(r.w.diff)[1] = "auth"
names(r.w.diff)[2] = "women.count"
names(r.w.diff)[3] = "men.count"
r.w.diff[4] <- r.w.diff$women - r.w.diff$men
names(r.w.diff)[4] = "raw.diff"
r.w.diff[5] <- r.w.diff$women.count / sum(r.w.diff$women.count) * 100
names(r.w.diff)[5] = "women.perc"
r.w.diff[6] <- r.w.diff$men.count / sum(r.w.diff$men.count) * 100
names(r.w.diff)[6] = "men.perc"
r.w.diff[7] <- r.w.diff$women.perc - r.w.diff$men.perc
names(r.w.diff)[7] = "perc.diff"

write.csv(r.w.diff, "Female_Auth_Diff.csv")



# freq table differences for male books

r.m.book.diff <- merge(w.r.m.book, m.r.m.book, by = "Var1", all = TRUE)
r.m.book.diff[is.na(r.m.book.diff)] <- 0
names(r.m.book.diff)[1] = "auth"
names(r.m.book.diff)[2] = "women.count"
names(r.m.book.diff)[3] = "men.count"
r.m.book.diff[4] <- r.m.book.diff$women - r.m.book.diff$men
names(r.m.book.diff)[4] = "raw.diff"
r.m.book.diff[5] <- r.m.book.diff$women.count / sum(r.m.book.diff$women.count) * 100
names(r.m.book.diff)[5] = "women.perc"
r.m.book.diff[6] <- r.m.book.diff$men.count / sum(r.m.book.diff$men.count) * 100
names(r.m.book.diff)[6] = "men.perc"
r.m.book.diff[7] <- r.m.book.diff$women.perc - r.m.book.diff$men.perc
names(r.m.book.diff)[7] = "perc.diff"

write.csv(r.m.book.diff, "Male_Book_Diff.csv")


# freq table differences for female books

w.m.book.diff <- merge(w.r.w.book, m.r.w.book, by = "Var1", all = TRUE)
w.m.book.diff[is.na(w.m.book.diff)] <- 0
names(w.m.book.diff)[1] = "auth"
names(w.m.book.diff)[2] = "women.count"
names(w.m.book.diff)[3] = "men.count"
w.m.book.diff[4] <- w.m.book.diff$women - w.m.book.diff$men
names(w.m.book.diff)[4] = "raw.diff"
w.m.book.diff[5] <- w.m.book.diff$women.count / sum(w.m.book.diff$women.count) * 100
names(w.m.book.diff)[5] = "women.perc"
w.m.book.diff[6] <- w.m.book.diff$men.count / sum(w.m.book.diff$men.count) * 100
names(w.m.book.diff)[6] = "men.perc"
w.m.book.diff[7] <- w.m.book.diff$women.perc - w.m.book.diff$men.perc
names(w.m.book.diff)[7] = "perc.diff"

write.csv(w.m.book.diff, "Female_Book_Diff.csv")



# frequency tables by members

m.r.m.memb <- as.data.frame(table(m.r.m$member_name))
names(m.r.m.memb)[1] = "memb"
m.r.w.memb <- as.data.frame(table(m.r.w$member_name))
names(m.r.w.memb)[1] = "memb"
w.r.m.memb <- as.data.frame(table(w.r.m$member_name))
names(w.r.m.memb)[1] = "memb"
w.r.w.memb <- as.data.frame(table(w.r.w$member_name))
names(w.r.w.memb)[1] = "memb"

write.csv(m.r.m.memb, "m_r_m_memb.csv")
write.csv(m.r.w.memb, "m_r_w_memb.csv")
write.csv(w.r.m.memb, "w_r_m_memb.csv")
write.csv(w.r.w.memb, "w_r_w_memb.csv")

# merge memb sheets

m.memb.ratios <- merge(m.r.m.memb, m.r.w.memb, by = "memb", all = TRUE)
m.memb.ratios[is.na(m.memb.ratios)] <- 0
names(m.memb.ratios)[2] <- "men"
names(m.memb.ratios)[3] <- "women"
m.memb.ratios[4] <- m.memb.ratios$men - m.memb.ratios$women
names(m.memb.ratios)[4] = "raw.diff"
m.memb.ratios[5] <- m.memb.ratios$women / sum(m.memb.ratios$women) * 100
names(m.memb.ratios)[5] = "women.perc"
m.memb.ratios[6] <- m.memb.ratios$men / sum(m.memb.ratios$men) * 100
names(m.memb.ratios)[6] = "men.perc"
m.memb.ratios[7] <- m.memb.ratios$men.perc - m.memb.ratios$women.perc
names(m.memb.ratios)[7] = "perc.diff"


w.memb.ratios <- merge(w.r.m.memb, w.r.w.memb, by = "memb", all = TRUE)
w.memb.ratios[is.na(w.memb.ratios)] <- 0
names(w.memb.ratios)[2] <- "men"
names(w.memb.ratios)[3] <- "women"
w.memb.ratios[4] <- w.memb.ratios$men - w.memb.ratios$women
names(w.memb.ratios)[4] = "raw.diff"
w.memb.ratios[5] <- w.memb.ratios$women / sum(w.memb.ratios$women) * 100
names(w.memb.ratios)[5] = "women.perc"
w.memb.ratios[6] <- w.memb.ratios$men / sum(w.memb.ratios$men) * 100
names(w.memb.ratios)[6] = "men.perc"
w.memb.ratios[7] <- w.memb.ratios$men.perc - w.memb.ratios$women.perc
names(w.memb.ratios)[7] = "perc.diff"

write.csv(m.memb.ratios, "male_reading_ratios.csv")
write.csv(w.memb.ratios, "female_reading_ratios.csv")











