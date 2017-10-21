# Week 3 Lab

Advertising <- read.csv("advertising.csv")
attach(Advertising)

TV_Radio <- TV*radio

lm.TV_Radio <- lm(sales~TV+radio+TV_Radio)
lm.Interaction <- lm(sales~TV*radio)
