#PACKAGES----
library(MASS)
library(car)
library(forcats)
library(MVN)
library(rms)
library(survival)
library(nnet)


#DATA MANIPULATION----

df <-
  read.delim('C:\\Users\\Utente\\Desktop\\Project\\Ranks\\Data\\rome1000.tsv',
             header = TRUE)

iter <- 1
score <- 10
for (i in seq_along(df$Rank)) {
  df$score[i] <- score
  iter <- iter + 1
  if (iter == 101) {
    score <- score - 1
    iter <- 1
  }
  df$score[df$score == 0] <- 1
}

df$score <- factor(df$score)
#View(df)

new_df <- df[, c(3, 6:20, 22, 24)]
#View(new_df)

new_df$Genius

new_df$Genius <- as.factor(new_df$Genius)
new_df$Preferred.Partner <- as.factor(new_df$Preferred.Partner)
new_df$Featured <- as.factor(new_df$Featured)
new_df$Deal <- as.factor(new_df$Deal)
new_df$Deal.type <- as.factor(new_df$Deal.type)
new_df$Stars <- as.factor(new_df$Stars)
new_df$Free.Cancellation <- as.factor(new_df$Free.Cancellation)
new_df$Breakfast.included <- as.factor(new_df$Breakfast.included)
new_df$Pay.later <- as.factor(new_df$Pay.later)
new_df$Travel.Sustainability <-
  as.factor(new_df$Travel.Sustainability)
new_df$Rental.type <-
  relevel(as.factor(new_df$Rental.type), ref = "Hotel")
new_df <- new_df[complete.cases(new_df),]


model <- polr(
  score ~ Nightly.Price * Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius * Preferred.Partner + Featured +
    Deal.type +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

#Warning: careful about overfitting...
#Interaction between ratings and its number produces 'Errore in svd(X) : valori infiniti o assenti in 'x''
#Featured produces multicollinearity
options(scipen = 999)
summary(model)
options(scipen = 0)

#We want to test the significance of interactions first, 
# exploiting the t-values obtained from the model 
t_inter <- list()
t_inter[['Nightly.Price:Discount..']] <- 1.185e+00
t_inter[['GeniusTrue:Preferred.PartnerPP']] <- 1.605e+00
t_inter[['GeniusTrue:Preferred.PartnerPP+']] <- 3.363e+00
t_inter[['Nightly.Price:Distance']] <- -3.677e+00
t_inter[['Nightly.Price:Stars2']] <- 1.247e+00
t_inter[['Nightly.Price:Stars3']] <- 4.583e+00
t_inter[['Nightly.Price:Stars4']] <- 5.569e+00
t_inter[['Nightly.Price:Stars5']] <- 1.200e+01
t_inter

#We choose a type I error alpha = 0.05 and exploit the asymptotic properties
# Now we can build confidence intervals for the parameters
# CI = [B_hat_k +- S.E. * 1.96]

CI_gen.pp <- c(3.946e-01 - 2.460e-01 * 1.96, 3.946e-01 + 2.460e-01 * 1.96)
paste(
  CI_gen.pp[1], 
  CI_gen.pp[2], 
  'Interaction between genius and preferred program is not significantly different from 0'
)

CI_gen.ppp <- c(8.265e-01 - 2.457e-01 * 1.96, 8.265e-01 + 2.457e-01 * 1.96)
paste(
  CI_gen.ppp[1], 
  CI_gen.ppp[2], 
  'Interaction between genius and preferred plus program is significantly different from 0'
)

CI_pr.st2 <- c(4.936e-03 - 3.957e-03 * 1.96, 4.936e-03 + 3.957e-03 * 1.96)
paste(
  CI_pr.st2[1], 
  CI_pr.st2[2], 
  'Interaction between price and 2 stars is not significantly different from 0'
)

CI_pr.st3 <- c(1.663e-02 - 3.628e-03 * 1.96, 1.663e-02 + 3.628e-03 * 1.96)
paste(
  CI_pr.st3[1], 
  CI_pr.st3[2], 
  'Interaction between price and 3 stars is significantly different from 0'
)

CI_pr.st4 <- c(2.006e-02 - 3.602e-03 * 1.96, 2.006e-02 + 3.602e-03 * 1.96)
paste(
  CI_pr.st4[1], 
  CI_pr.st4[2], 
  'Interaction between price and 4 stars is significantly different from 0 (though close to 0)'
)

CI_pr.st5 <- c(4.518e-02 - 3.766e-03 * 1.96, 4.518e-02 + 3.766e-03 * 1.96)
paste(
  CI_pr.st5[1], 
  CI_pr.st5[2], 
  'Interaction between price and 5 stars is significantly different from 0 (though close to 0)'
)

#Interpretation of interactions: 

# We notice that for the interactions between price and stars, the coefficients are positive
# meaning that, fixed the number of stars, the log odds for the score increase of the estimated coefficient
# as price increases.
# That is, as price increases, the ranking system will derank the property.
# What are the possible reasons for the increase of the estimated coefficient as the number of stars increase?
# Guest have higher expectations for high star rated hotels. Thus, small price changes may largely affect value perception.
# High star rated hotels live in a more competitive environement in which even small price changes can lead to increase in value perception thus to increased views, booking, etc.

# But why is the interaction with 2 stars not significant?
# It may be due to a small sample size for this specific category of properties,
# which may lead to larger variance and therefore larger confidence intervals.
# Let's check this

for (i in 1:5){
  by_stars <- df[df$Stars == i,]
  print(paste('For ', i, ' stars we have ', nrow(by_stars), ' units'))
}

#We notice, in fact, that the number of units for the following categories:
# 1 star, 2 stars and 5 stars
# is small. Therefore, we choose to merge in two subgroups (1-3, 4-5)

new_df$Stars <- as.numeric(new_df$Stars)
new_df$Stars[new_df$Stars %in% c(1, 2, 3)] <- "Low"
new_df$Stars[new_df$Stars %in% c(4, 5)] <- "High"
new_df$Stars <- as.factor(new_df$Stars)
levels(new_df$Stars)
table(new_df$Stars)

new_df$Stars <-
  relevel(as.factor(new_df$Stars), ref = "Low")

#Refit the model with stars as a binary factor
model <- polr(
  score ~ Nightly.Price * Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius * Preferred.Partner + Featured +
    Deal.type +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model)
CI_pr.stars <- c(0.0055366 - 0.0018510908 * 1.96, 0.0055366 + 0.0018510908 * 1.96)
paste(
  CI_pr.st5[1], 
  CI_pr.st5[2], 
  'Interaction between price and high stars is significantly different from 0 (though close to 0)'
)

#There is a small positive interaction
#It suggests that the effect of price on the score is slightly stronger when the level of stars is high compared to when it is low
#It may mean that higher prices are more justified by higher stars inside the recommender system compared to low stars.

#We decide to keep this interaction term, thus we also keep the stars regressor.

#The second interaction term is Nightly.Price:Distance

CI_pr.dist <- c(-0.0024366 - 0.0008128412 * 1.96, -0.0024366 + 0.0008128412 * 1.96)
paste(
  CI_pr.dist[1], 
  CI_pr.dist[2], 
  'Interaction between price and distance is significantly different from 0 (though close to 0)'
)

#The coefficient is slightly less than 0 at a 95% level of confidence
#This suggests that the effect of price on the score is stronger when we also increase distance.
#That is, a higher price negatively affects the score even more when the rental is far from the centre, compared to close ones.
#As before, the recommender system may justify some high prices with closeness to the centre.


#We notice though that the lower bound is very close to 0
#Since the effect is not straightforward, we decide to use AIC
#to decide if we keep it in the model or not
#We therefore remove this interaction term from our model
#and fit the new model

model2 <- polr(
  score ~ Nightly.Price * Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius * Preferred.Partner + Featured +
    Deal.type +
    Distance + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model2)
summary(model)

#The model with the interaction term has a lower AIC, so we go with it

#Let's now work with the price-discount interaction term
CI_pr.disc <- c(0.0037142 - 2.788e-03 * 1.96, 0.0037142 + 2.788e-03 * 1.96)
paste(
  CI_pr.disc[1], 
  CI_pr.disc[2], 
  'Interaction between price and discount is not significantly different from 0'
)

#This means that there is no significant change in the score when we increase or decrease price
#while applying a discount at the same time.
#Thus, we disregard this interaction term automatically.

#We just double check AIC
model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius * Preferred.Partner + Featured +
    Deal.type +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)
summary(model)

#The AIC has infact decreased compared to our last model fit,
# giving us more reason to reduce the model

#However, we also want to check our data.
summary(new_df$Discount..)
hist(new_df$Discount..)

#75% of observations have a discount less that 17%.
#Infact, we have very few data with a large discount (limited time offer)
length(new_df$Discount..[new_df$Discount.. > 0.3])
length(new_df$Discount..[new_df$Deal.type == 'Offerta a tempo limitato'])
#45 properties have a discount greater than 30%. 
#However, only 2 of these have a limited time offer deal.
#Furthermore, only 13 have a secret offer.
#Thus, we decide to get rid of the deal type variable and 
#only use the discount numeric variable, as we have not enough data.

levels(new_df$Deal.type)
length(new_df$Discount..[new_df$Deal.type == 'Offerta a tempo limitato'])
length(new_df$Discount..[new_df$Deal.type == 'Offerta SuperSegreta'])
length(new_df$Discount..[new_df$Deal.type == 'None'])

#However, we may be convinced that a limited time deal improves score substantially in the short term.
#We proceed by removing the Deal type regressor

model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius * Preferred.Partner + Featured +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)
summary(model)

#We notice that the AIC has been increased by a quite important amount.
#This is because we are unexplaining a big portion of the variance of the score
#dependent on the deal type. Practically, this regressor has a big effect.
#However, for statistical significance, we choose not to rely just on few data.

#Getting towards the end of the analysis of interaction term, 
# we now consider the effect of Booking.com's partnership programmes.
#First of all, let's check the 95% confidence intervals give the updated model

CI_gen.pp <- c(0.5418861 - 0.2435638 * 1.96, 0.5418861 + 0.2435638 * 1.96)
paste(
  CI_gen.pp[1], 
  CI_gen.pp[2], 
  'Interaction between genius and preferred program is significantly different from 0, with quite a large variance'
)

CI_gen.ppp <- c(1.1987279 - 0.2439138 * 1.96, 1.1987279 + 0.2439138 * 1.96)
paste(
  CI_gen.ppp[1], 
  CI_gen.ppp[2], 
  'Interaction between genius and preferred plus program is significantly different from 0, with large variance but some good level of certainty'
)

#We may easily conclude that the interaction between the two programmes has a positive effect on the score.
#Participating to Genius Programme and being a Preffered Partner Plus gives a substantial boost to your score.
#The lower bound of this interaction is 0.72.

#We must be more careful about the first interaction term,
#as the large variance brings the estimate close to 0.

#We also consider another aspect: participation to the programmes (especially the preferred partner)
#is allowed under certain restrictions, which can be quite elective.
#It can be possible that instead of the interaction having an effect on the score,
#it is the score that has an effect on the participation to the programmes.

#We want to avoid adding second outcomes to the model. 
#Thus, we check AIC as a last stage.
model2 <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner + Featured +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model2)
summary(model)

#The AIC (minimally) decreases for the full model.
#Thus, we are motivated to believe in our theory of the interaction
#as a second outcome and decide to remove it from the model for parsimony.

model <- model2

#For the moment, we have concluded that only two interactions,
#Distance:Nightly.Price - Nightly.Price:StarsHigh
#have a significant effect on the dependent variable score
#We can now study the effect of the single regressors on the model.

#We start with defining 95% confidence intervals for each regressor

summary(model)

# Confidence interval for Discount..
CI_disc <- c(0.3275356 - 0.0363124 * 1.96, 0.3275356 + 0.0363124 * 1.96)
paste(
  CI_disc[1], 
  CI_disc[2], 
  'Discount is significantly different from 0'
)

# Confidence interval for Ratings
CI_rating <- c(0.6130246 - 0.0875855 * 1.96, 0.6130246 + 0.0875855 * 1.96)
paste(
  CI_rating[1], 
  CI_rating[2], 
  'Ratings is significantly different from 0'
)

# Confidence interval for Number.of.Reviews
CI_numrev <- c(-0.0002025 - 0.0001154 * 1.96, -0.0002025 + 0.0001154 * 1.96)
paste(
  CI_numrev[1], 
  CI_numrev[2], 
  'Number of reviews is not significantly different from 0'
)

# Confidence interval for Location.Ratings
CI_locrating <- c(0.3732599 - 0.0841072 * 1.96, 0.3732599 + 0.0841072 * 1.96)
paste(
  CI_locrating[1], 
  CI_locrating[2], 
  'Location Ratings is significantly different from 0'
)

# Confidence interval for GeniusTrue
CI_genius <- c(2.4244721 - 0.1715780 * 1.96, 2.4244721 + 0.1715780 * 1.96)
paste(
  CI_genius[1], 
  CI_genius[2], 
  'GeniusTrue is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP
CI_pp <- c(2.5694657 - 0.1991094 * 1.96, 2.5694657 + 0.1991094 * 1.96)
paste(
  CI_pp[1], 
  CI_pp[2], 
  'Preferred Partner (PP) is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP+
CI_ppplus <- c(5.5787424 - 0.3163255 * 1.96, 5.5787424 + 0.3163255 * 1.96)
paste(
  CI_ppplus[1], 
  CI_ppplus[2], 
  'Preferred Partner (PP+) is significantly different from 0'
)

# Confidence interval for FeaturedTrue
CI_featured <- c(-0.4237116 - 0.4225802 * 1.96, -0.4237116 + 0.4225802 * 1.96)
paste(
  CI_featured[1], 
  CI_featured[2], 
  'FeaturedTrue is not significantly different from 0'
)

# Confidence interval for Distance
CI_distance <- c(-0.1849260 - 0.1334377 * 1.96, -0.1849260 + 0.1334377 * 1.96)
paste(
  CI_distance[1], 
  CI_distance[2], 
  'Distance is not significantly different from 0'
)

# Confidence interval for Nightly.Price
CI_price <- c(-0.0264573 - 0.0023590 * 1.96, -0.0264573 + 0.0023590 * 1.96)
paste(
  CI_price[1], 
  CI_price[2], 
  'Nightly Price is significantly different from 0'
)

# Confidence interval for StarsHigh
CI_stars <- c(-0.8180740 - 0.4498642 * 1.96, -0.8180740 + 0.4498642 * 1.96)
paste(
  CI_stars[1], 
  CI_stars[2], 
  'StarsHigh is not significantly different from 0'
)

# Confidence interval for Free.CancellationTrue
CI_cancel <- c(1.5568560 - 0.1383077 * 1.96, 1.5568560 + 0.1383077 * 1.96)
paste(
  CI_cancel[1], 
  CI_cancel[2], 
  'Free Cancellation is significantly different from 0'
)

# Confidence interval for Breakfast.includedTrue
CI_breakfast <- c(0.0679373 - 0.1439810 * 1.96, 0.0679373 + 0.1439810 * 1.96)
paste(
  CI_breakfast[1], 
  CI_breakfast[2], 
  'Breakfast included is not significantly different from 0'
)

# Confidence interval for Pay.laterTrue
CI_paylater <- c(0.5903955 - 0.2639323 * 1.96, 0.5903955 + 0.2639323 * 1.96)
paste(
  CI_paylater[1], 
  CI_paylater[2], 
  'Pay later is significantly different from 0'
)

# Confidence interval for Travel.Sustainability1
CI_sustain1 <- c(-0.2882349 - 0.1365113 * 1.96, -0.2882349 + 0.1365113 * 1.96)
paste(
  CI_sustain1[1], 
  CI_sustain1[2], 
  'Travel Sustainability 1 is significantly different from 0'
)

# Confidence interval for Travel.Sustainability2
CI_sustain2 <- c(-0.4271543 - 0.1857235 * 1.96, -0.4271543 + 0.1857235 * 1.96)
paste(
  CI_sustain2[1], 
  CI_sustain2[2], 
  'Travel Sustainability 2 is significantly different from 0'
)

# Confidence interval for Travel.Sustainability3
CI_sustain3 <- c(-0.6641957 - 0.4035679 * 1.96, -0.6641957 + 0.4035679 * 1.96)
paste(
  CI_sustain3[1], 
  CI_sustain3[2], 
  'Travel Sustainability 3 is not significantly different from 0'
)

# Confidence interval for Rental.typeAffittacamere
CI_affittacamere <- c(-0.8282876 - 0.1767120 * 1.96, -0.8282876 + 0.1767120 * 1.96)
paste(
  CI_affittacamere[1], 
  CI_affittacamere[2], 
  'Rental type Affittacamere is significantly different from 0'
)
# Confidence interval for Rental.typeAffittacamere
CI_affittacamere <- c(-0.8282876 - 0.1767120 * 1.96, -0.8282876 + 0.1767120 * 1.96)
paste(
  CI_affittacamere[1], 
  CI_affittacamere[2], 
  'Rental type Affittacamere is significantly different from 0'
)

# Confidence interval for Rental.typeAlloggio in famiglia
CI_alloggio <- c(-2.3385819 - 0.0210843 * 1.96, -2.3385819 + 0.0210843 * 1.96)
paste(
  CI_alloggio[1], 
  CI_alloggio[2], 
  'Rental type Alloggio in famiglia is significantly different from 0'
)

# Confidence interval for Rental.typeAppartamento
CI_appartamento <- c(-1.8216518 - 0.2823307 * 1.96, -1.8216518 + 0.2823307 * 1.96)
paste(
  CI_appartamento[1], 
  CI_appartamento[2], 
  'Rental type Appartamento is significantly different from 0'
)

# Confidence interval for Rental.typeBed & breakfast
CI_bedbreak <- c(-0.8613088 - 0.1989029 * 1.96, -0.8613088 + 0.1989029 * 1.96)
paste(
  CI_bedbreak[1], 
  CI_bedbreak[2], 
  'Rental type Bed & breakfast is significantly different from 0'
)

# Confidence interval for Rental.typeCasa vacanze
CI_casavacanze <- c(-3.3988862 - 0.0111405 * 1.96, -3.3988862 + 0.0111405 * 1.96)
paste(
  CI_casavacanze[1], 
  CI_casavacanze[2], 
  'Rental type Casa vacanze is significantly different from 0'
)

# Confidence interval for Rental.typeOstello
CI_ostello <- c(-1.4359440 - 0.0146636 * 1.96, -1.4359440 + 0.0146636 * 1.96)
paste(
  CI_ostello[1], 
  CI_ostello[2], 
  'Rental type Ostello is significantly different from 0'
)

# Confidence interval for Rental.typeResidence
CI_residence <- c(-0.5114144 - 0.0411681 * 1.96, -0.5114144 + 0.0411681 * 1.96)
paste(
  CI_residence[1], 
  CI_residence[2], 
  'Rental type Residence is significantly different from 0'
)

# Confidence interval for Rental.typeVillaggio turistico
CI_villaggio <- c(5.9097261 - 0.0376398 * 1.96, 5.9097261 + 0.0376398 * 1.96)
paste(
  CI_villaggio[1], 
  CI_villaggio[2], 
  'Rental type Villaggio turistico is significantly different from 0'
)

#We proceed by removing Featured

model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Breakfast.included + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model)

#The AIC has decreased too
#Let's check the coefficients again

#Coefficients' CI's after we removed featured----
# Confidence interval for Discount..
CI_disc <- c(0.3024560 - 0.0352086 * 1.96, 0.3024560 + 0.0352086 * 1.96)
paste(
  CI_disc[1], 
  CI_disc[2], 
  'Discount is significantly different from 0'
)

# Confidence interval for Ratings
CI_rating <- c(0.6155676 - 0.0874942 * 1.96, 0.6155676 + 0.0874942 * 1.96)
paste(
  CI_rating[1], 
  CI_rating[2], 
  'Ratings is significantly different from 0'
)

# Confidence interval for Number.of.Reviews
CI_numrev <- c(-0.0002011 - 0.0001155 * 1.96, -0.0002011 + 0.0001155 * 1.96)
paste(
  CI_numrev[1], 
  CI_numrev[2], 
  'Number of reviews is not significantly different from 0'
)

# Confidence interval for Location.Ratings
CI_locrating <- c(0.3660342 - 0.0839568 * 1.96, 0.3660342 + 0.0839568 * 1.96)
paste(
  CI_locrating[1], 
  CI_locrating[2], 
  'Location Ratings is significantly different from 0'
)

# Confidence interval for GeniusTrue
CI_genius <- c(2.4263471 - 0.1712379 * 1.96, 2.4263471 + 0.1712379 * 1.96)
paste(
  CI_genius[1], 
  CI_genius[2], 
  'GeniusTrue is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP
CI_pp <- c(2.5793615 - 0.1986271 * 1.96, 2.5793615 + 0.1986271 * 1.96)
paste(
  CI_pp[1], 
  CI_pp[2], 
  'Preferred Partner (PP) is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP+
CI_ppplus <- c(5.5917895 - 0.3157730 * 1.96, 5.5917895 + 0.3157730 * 1.96)
paste(
  CI_ppplus[1], 
  CI_ppplus[2], 
  'Preferred Partner (PP+) is significantly different from 0'
)

# Confidence interval for Distance
CI_distance <- c(-0.1832765 - 0.1333009 * 1.96, -0.1832765 + 0.1333009 * 1.96)
paste(
  CI_distance[1], 
  CI_distance[2], 
  'Distance is not significantly different from 0'
)

# Confidence interval for Nightly.Price
CI_price <- c(-0.0264011 - 0.0023550 * 1.96, -0.0264011 + 0.0023550 * 1.96)
paste(
  CI_price[1], 
  CI_price[2], 
  'Nightly Price is significantly different from 0'
)

# Confidence interval for StarsHigh
CI_stars <- c(-0.8143252 - 0.4502002 * 1.96, -0.8143252 + 0.4502002 * 1.96)
paste(
  CI_stars[1], 
  CI_stars[2], 
  'StarsHigh is not significantly different from 0'
)

# Confidence interval for Free.CancellationTrue
CI_cancel <- c(1.5597746 - 0.1382344 * 1.96, 1.5597746 + 0.1382344 * 1.96)
paste(
  CI_cancel[1], 
  CI_cancel[2], 
  'Free Cancellation is significantly different from 0'
)

# Confidence interval for Breakfast.includedTrue
CI_breakfast <- c(0.0719875 - 0.1438292 * 1.96, 0.0719875 + 0.1438292 * 1.96)
paste(
  CI_breakfast[1], 
  CI_breakfast[2], 
  'Breakfast included is not significantly different from 0'
)

# Confidence interval for Pay.laterTrue
CI_paylater <- c(0.5957569 - 0.2637860 * 1.96, 0.5957569 + 0.2637860 * 1.96)
paste(
  CI_paylater[1], 
  CI_paylater[2], 
  'Pay later is significantly different from 0'
)

# Confidence interval for Travel.Sustainability1
CI_sustain1 <- c(-0.2899493 - 0.1365139 * 1.96, -0.2899493 + 0.1365139 * 1.96)
paste(
  CI_sustain1[1], 
  CI_sustain1[2], 
  'Travel Sustainability 1 is not significantly different from 0'
)

# Confidence interval for Travel.Sustainability2
CI_sustain2 <- c(-0.4444440 - 0.1848110 * 1.96, -0.4444440 + 0.1848110 * 1.96)
paste(
  CI_sustain2[1], 
  CI_sustain2[2], 
  'Travel Sustainability 2 is not significantly different from 0'
)

# Confidence interval for Travel.Sustainability3
CI_sustain3 <- c(-0.6611062 - 0.4037962 * 1.96, -0.6611062 + 0.4037962 * 1.96)
paste(
  CI_sustain3[1], 
  CI_sustain3[2], 
  'Travel Sustainability 3 is not significantly different from 0'
)

# Confidence interval for Rental.typeAffittacamere
CI_affittacamere <- c(-0.8324326 - 0.1765064 * 1.96, -0.8324326 + 0.1765064 * 1.96)
paste(
  CI_affittacamere[1], 
  CI_affittacamere[2], 
  'Rental type Affittacamere is significantly different from 0'
)

# Confidence interval for Rental.typeAlloggio in famiglia
CI_alloggio <- c(-2.3293495 - 0.0211172 * 1.96, -2.3293495 + 0.0211172 * 1.96)
paste(
  CI_alloggio[1], 
  CI_alloggio[2], 
  'Rental type Alloggio in famiglia is significantly different from 0'
)

# Confidence interval for Rental.typeAppartamento
CI_appartamento <- c(-1.8226831 - 0.2821591 * 1.96, -1.8226831 + 0.2821591 * 1.96)
paste(
  CI_appartamento[1], 
  CI_appartamento[2], 
  'Rental type Appartamento is significantly different from 0'
)

# Confidence interval for Rental.typeBed & breakfast
CI_bedbreak <- c(-0.8657300 - 0.1986989 * 1.96, -0.8657300 + 0.1986989 * 1.96)
paste(
  CI_bedbreak[1], 
  CI_bedbreak[2], 
  'Rental type Bed & breakfast is significantly different from 0'
)

# Confidence interval for Rental.typeCasa vacanze
CI_casavacanze <- c(-3.3900552 - 0.0111602 * 1.96, -3.3900552 + 0.0111602 * 1.96)
paste(
  CI_casavacanze[1], 
  CI_casavacanze[2], 
  'Rental type Casa vacanze is significantly different from 0'
)

# Confidence interval for Rental.typeOstello
CI_ostello <- c(-1.4416680 - 0.0146497 * 1.96, -1.4416680 + 0.0146497 * 1.96)
paste(
  CI_ostello[1], 
  CI_ostello[2], 
  'Rental type Ostello is significantly different from 0'
)

# Confidence interval for Rental.typeResidence
CI_residence <- c(-0.5027125 - 0.0412247 * 1.96, -0.5027125 + 0.0412247 * 1.96)
paste(
  CI_residence[1], 
  CI_residence[2], 
  'Rental type Residence is significantly different from 0'
)

# Confidence interval for Rental.typeVillaggio turistico
CI_villaggio <- c(5.8778746 - 0.0376280 * 1.96, 5.8778746 + 0.0376280 * 1.96)
paste(
  CI_villaggio[1], 
  CI_villaggio[2], 
  'Rental type Villaggio turistico is significantly different from 0'
)

# Confidence interval for Distance:Nightly.Price
CI_distnight <- c(-0.0023241 - 0.0008071 * 1.96, -0.0023241 + 0.0008071 * 1.96)
paste(
  CI_distnight[1], 
  CI_distnight[2], 
  'Interaction between Distance and Nightly Price is significantly different from 0'
)

# Confidence interval for Nightly.Price:StarsHigh
CI_pricestars <- c(0.0058668 - 0.0018809 * 1.96, 0.0058668 + 0.0018809 * 1.96)
paste(
  CI_pricestars[1], 
  CI_pricestars[2], 
  'Interaction between Nightly Price and StarsHigh is significantly different from 0'
)

#We now remove Breakfast, which is probably 0 in the model
model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model)

#The AIC has slightly decreased. We are happy with this removal

#Coefficients' CI's after we remove Breakfast----
# Confidence interval for Discount..
CI_disc <- c(0.3159133 - 0.0358071 * 1.96, 0.3159133 + 0.0358071 * 1.96)
paste(
  CI_disc[1], 
  CI_disc[2], 
  'Discount is significantly different from 0'
)

# Confidence interval for Ratings
CI_rating <- c(0.6154095 - 0.0874869 * 1.96, 0.6154095 + 0.0874869 * 1.96)
paste(
  CI_rating[1], 
  CI_rating[2], 
  'Ratings is significantly different from 0'
)

# Confidence interval for Number.of.Reviews
CI_numrev <- c(-0.0001984 - 0.0001154 * 1.96, -0.0001984 + 0.0001154 * 1.96)
paste(
  CI_numrev[1], 
  CI_numrev[2], 
  'Number of Reviews is not significantly different from 0'
)

# Confidence interval for Location.Ratings
CI_locrating <- c(0.3636933 - 0.0838456 * 1.96, 0.3636933 + 0.0838456 * 1.96)
paste(
  CI_locrating[1], 
  CI_locrating[2], 
  'Location Ratings is significantly different from 0'
)

# Confidence interval for GeniusTrue
CI_genius <- c(2.4196481 - 0.1710483 * 1.96, 2.4196481 + 0.1710483 * 1.96)
paste(
  CI_genius[1], 
  CI_genius[2], 
  'GeniusTrue is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP
CI_pp <- c(2.5770236 - 0.1986579 * 1.96, 2.5770236 + 0.1986579 * 1.96)
paste(
  CI_pp[1], 
  CI_pp[2], 
  'Preferred Partner (PP) is significantly different from 0'
)

# Confidence interval for Preferred.PartnerPP+
CI_ppplus <- c(5.5850737 - 0.3155790 * 1.96, 5.5850737 + 0.3155790 * 1.96)
paste(
  CI_ppplus[1], 
  CI_ppplus[2], 
  'Preferred Partner (PP+) is significantly different from 0'
)

# Confidence interval for Distance
CI_distance <- c(-0.1778050 - 0.1330497 * 1.96, -0.1778050 + 0.1330497 * 1.96)
paste(
  CI_distance[1], 
  CI_distance[2], 
  'Distance is not significantly different from 0'
)

# Confidence interval for Nightly.Price
CI_price <- c(-0.0262567 - 0.0023386 * 1.96, -0.0262567 + 0.0023386 * 1.96)
paste(
  CI_price[1], 
  CI_price[2], 
  'Nightly Price is significantly different from 0'
)

# Confidence interval for StarsHigh
CI_stars <- c(-0.8044562 - 0.4491866 * 1.96, -0.8044562 + 0.4491866 * 1.96)
paste(
  CI_stars[1], 
  CI_stars[2], 
  'StarsHigh is significantly different from 0'
)

# Confidence interval for Free.CancellationTrue
CI_cancel <- c(1.5562664 - 0.1381144 * 1.96, 1.5562664 + 0.1381144 * 1.96)
paste(
  CI_cancel[1], 
  CI_cancel[2], 
  'Free Cancellation is significantly different from 0'
)

# Confidence interval for Pay.laterTrue
CI_paylater <- c(0.5935883 - 0.2636691 * 1.96, 0.5935883 + 0.2636691 * 1.96)
paste(
  CI_paylater[1], 
  CI_paylater[2], 
  'Pay later is significantly different from 0'
)

# Confidence interval for Travel.Sustainability1
CI_sustain1 <- c(-0.2904546 - 0.1364794 * 1.96, -0.2904546 + 0.1364794 * 1.96)
paste(
  CI_sustain1[1], 
  CI_sustain1[2], 
  'Travel Sustainability 1 is not significantly different from 0'
)

# Confidence interval for Travel.Sustainability2
CI_sustain2 <- c(-0.4485921 - 0.1845802 * 1.96, -0.4485921 + 0.1845802 * 1.96)
paste(
  CI_sustain2[1], 
  CI_sustain2[2], 
  'Travel Sustainability 2 is not significantly different from 0'
)

# Confidence interval for Travel.Sustainability3
CI_sustain3 <- c(-0.6498721 - 0.4028426 * 1.96, -0.6498721 + 0.4028426 * 1.96)
paste(
  CI_sustain3[1], 
  CI_sustain3[2], 
  'Travel Sustainability 3 is not significantly different from 0'
)

# Confidence interval for Rental.typeAffittacamere
CI_affittacamere <- c(-0.8365246 - 0.1763027 * 1.96, -0.8365246 + 0.1763027 * 1.96)
paste(
  CI_affittacamere[1], 
  CI_affittacamere[2], 
  'Rental type Affittacamere is significantly different from 0'
)

# Confidence interval for Rental.typeAlloggio in famiglia
CI_alloggio <- c(-2.3461347 - 0.0211591 * 1.96, -2.3461347 + 0.0211591 * 1.96)
paste(
  CI_alloggio[1], 
  CI_alloggio[2], 
  'Rental type Alloggio in famiglia is significantly different from 0'
)

# Confidence interval for Rental.typeAppartamento
CI_appartamento <- c(-1.8338978 - 0.2811837 * 1.96, -1.8338978 + 0.2811837 * 1.96)
paste(
  CI_appartamento[1], 
  CI_appartamento[2], 
  'Rental type Appartamento is significantly different from 0'
)

# Confidence interval for Rental.typeBed & breakfast
CI_bedbreak <- c(-0.8556479 - 0.1974985 * 1.96, -0.8556479 + 0.1974985 * 1.96)
paste(
  CI_bedbreak[1], 
  CI_bedbreak[2], 
  'Rental type Bed & breakfast is significantly different from 0'
)

# Confidence interval for Rental.typeCasa vacanze
CI_casavacanze <- c(-3.4042028 - 0.0112039 * 1.96, -3.4042028 + 0.0112039 * 1.96)
paste(
  CI_casavacanze[1], 
  CI_casavacanze[2], 
  'Rental type Casa vacanze is significantly different from 0'
)

# Confidence interval for Rental.typeOstello
CI_ostello <- c(-1.3783815 - 0.0135900 * 1.96, -1.3783815 + 0.0135900 * 1.96)
paste(
  CI_ostello[1], 
  CI_ostello[2], 
  'Rental type Ostello is significantly different from 0'
)

# Confidence interval for Rental.typeResidence
CI_residence <- c(-0.5267934 - 0.0416904 * 1.96, -0.5267934 + 0.0416904 * 1.96)
paste(
  CI_residence[1], 
  CI_residence[2], 
  'Rental type Residence is significantly different from 0'
)

# Confidence interval for Rental.typeVillaggio turistico
CI_villaggio <- c(5.7817203 - 0.0378578 * 1.96, 5.7817203 + 0.0378578 * 1.96)
paste(
  CI_villaggio[1], 
  CI_villaggio[2], 
  'Rental type Villaggio turistico is significantly different from 0'
)

#We 



#We are not so convinced about the results for Travel Sustainability and its
#effect on the score.
#We decide to study further the variable.

table(new_df$Travel.Sustainability)

#We have very few properties with high travel sustainability (3)
#We decide to consider levels 2 and 3 'High Sustainability' and 
#merge the two groups for more consistent estimates

new_df$Travel.Sustainability[new_df$Travel.Sustainability == 3] <- 2
new_df$Travel.Sustainability <- factor(new_df$Travel.Sustainability, labels = c('None','Low','High'))
table(new_df$Travel.Sustainability)

#We refit the model with this new variable

model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model)

#The AIC has decreased
#Let's check confidence intervals for travel sustainability

# Confidence interval for Travel.Sustainability Low
CI_sustain_low <- c(-0.2892618 - 0.1364735 * 1.96, -0.2892618 + 0.1364735 * 1.96)
paste(
  CI_sustain_low[1], 
  CI_sustain_low[2], 
  'Travel Sustainability Low is significantly different from 0'
)

# Confidence interval for Travel.Sustainability High
CI_sustain_high <- c(-0.4765388 - 0.1751232 * 1.96, -0.4765388 + 0.1751232 * 1.96)
paste(
  CI_sustain_high[1], 
  CI_sustain_high[2], 
  'Travel Sustainability High is significantly different from 0'
)

#We keep them in the model
confint(model)
#Coefficients' CI's after we remove Number of Reviews----

model <- polr(
  score ~ Discount.. +
    Ratings + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

summary(model)

#We notice right away that the AIC has increased.
#Furthermore, we notice that the standard error for the variable
#Discount has increased a lot, making the appear latter not significant as well

#Thus, we believe that Number of Reviews is an important variable explaining
#a part of the variance that could not be explained otherwise. 
#So we decide to keep it in the model.

model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df,
  Hess = TRUE
)

summary(model)

#The other non significant variable is distance.
#However, we are encouraged to keep this variable in the model
#as its interaction with nightly price is significant

#Diagnostics: VIF----
#We check Variance Inflation Factors using the vif method for lm objects
#So we fit a linear model with the same regressors.

lm_model <- lm(
  as.numeric(score) ~ Discount.. +
    Ratings + Number.of.Reviews + Location.Ratings +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df
)

car::vif(lm_model, type = 'predictor')
car::vif(model)

#They are all less than 10, so we can conclude there is no multicollinearity

#Question: why different results on different models? Isn't it always regression coefficiet of a linear model?----

#Interpretation: COEFFICIENTS----
#Let's compute the final Confidence Intervals

#In order to have a good Hessian Matrix Approximation, we
#standardize the numeric variables in the model

numeric_cols <- c("Nightly.Price", "Distance", "Discount..", "Ratings", "Number.of.Reviews", "Location.Ratings")
numeric_data <- new_df[, numeric_cols]

scaled_data <- scale(numeric_data)
scaled_df <- as.data.frame(scaled_data)
names(scaled_df) <- c("S.Nightly.Price", "S.Distance", "S.Discount..", "S.Ratings", "S.Number.of.Reviews", "S.Location.Ratings")
new_df <- cbind(new_df, scaled_df)

model.S <- polr(
  score ~ S.Discount.. +
    S.Ratings + S.Number.of.Reviews + S.Location.Ratings +
    Genius + Preferred.Partner +
    S.Distance*S.Nightly.Price + Stars*S.Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df,
  Hess = TRUE
)

summary(model.S)


confint(model.S)

#Discount does not seem significant in its standardized version
#It may be because standardization reduces the gap between large discounts and small ones

model.S2 <- polr(
  score ~ 
    S.Ratings + S.Number.of.Reviews + S.Location.Ratings +
    Genius + Preferred.Partner +
    S.Distance*S.Nightly.Price + Stars*S.Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df,
  Hess = TRUE
)

summary(model.S2)

confint(model.S2)

#In fact, the AIC has dropped after its removal
#Question: WHY?----

#In non-standardized model we obtain: 
#Discount has a positive effect on score.

CI_disc <- c(0.3169349 - 0.0360687 * 1.96, 0.3169349 + 0.0360687 * 1.96)
paste(
  CI_disc[1], 
  CI_disc[2], 
  'Discount is significantly different from 0'
)

# That is, after a unit increase in discount (which is between 0 and 1 practically)
# the multiplicative effect of this on the odds of being ranked a certain category is between
exp(c(0.246240248,  0.3876296))

#Practically, the effect is positive but not that large,
#as for this variable we are in the scale of decimals


#Ratings have a positive effect on score.
# That is, after a unit increase in ratings (which is between 0 and 10 practically)
# the multiplicative effect of this on the odds of being ranked a certain category is between
exp(c(0.445379 ,  0.7883198))

#We can say ratings have quite a large role in the ranking of a property
#A rating of 9 instead of 8 has between 1.56 and 2.20 times the odds
#of being classified at a higher level


#Number of Reviews does not have a clear role on the dependent variable.
#We have decided to keep it in the model because we found an important
#impact on the coefficient of the Discount regressor.
#However, we failed to find a practical relationship between the two.

#DIAGNOSTICS----

#For a cumulative logit model, we find the coefficients through
#maximum likelihood estimation. To do this, we assumed that 
#the matrix X of variables is an independent MVN distribution.
#Let's check this for the numeric variables.

#Question: what happens for the categorical variables?----


numeric_df <- new_df[,c('Discount..', 'Ratings', 'Number.of.Reviews', 'Location.Ratings')]
mvn_test <- mvn(data = numeric_df, multivariatePlot = "qq")
print(mvn_test)

#According to these results, the data do not follow a Multivariate Normal Distribution
#nor does any variable follow a Univariate Normal distribution

#Let's check for the standardized data if we get better results.

numeric_df_scaled <- scale(new_df[,c('Discount..', 'Ratings', 'Number.of.Reviews', 'Location.Ratings')])
mvn_test_scaled <- mvn(data = numeric_df_scaled, multivariatePlot = "qq")
print(mvn_test_scaled)

#The p-values do not change

#Let's grafically check the distribution of our variables
hist(numeric_df[,'Discount..'])
hist(numeric_df_scaled[,'Discount..'])

hist(numeric_df[,'Ratings'])
hist(numeric_df_scaled[,'Ratings'])

hist(numeric_df[,'Number.of.Reviews'])
hist(numeric_df_scaled[,'Number.of.Reviews'])

hist(numeric_df[,'Location.Ratings'])
hist(numeric_df_scaled[,'Location.Ratings'])

#As we can see, the distributions are not at all normal, except Ratings
#which is the closest one.

#One main problem may be that each of these variables is limited to a range of values.
#That is,
#Discount belongs to [0,1]
#Ratings belongs to [0,10]
#Number of Reviews belongs to [0, +inf]
#Location Ratings belongs to [0,10]

#We can try applying some transformation to each variable 
#in order to get closer to a normal distribution

numeric_df <- new_df[,c('Discount..', 'Ratings', 'Number.of.Reviews', 'Location.Ratings')]

discount <- numeric_df$Discount..
hist(discount)

#Since the skewness is positive we apply these transformations

log_discount <- log(discount, base=10)
hist(log_discount)

sqrt_discount <- sqrt(discount)
hist(sqrt_discount, 10)

#The square root is a better approximation of a Normal
#However, it is still bounded below by 0

#

ratings <- numeric_df$Ratings
hist(ratings)

#We search for the optimal Box-Cox transform

boxcox_results <- boxcox(Ratings ~ 1, data = numeric_df)
plot(boxcox_results)

optimal_lambda <- 2

transformed_ratings <- (numeric_df$Ratings^optimal_lambda - 1) / optimal_lambda

hist(transformed_ratings,10)
#May be a better approximation

#

location <- new_df$Location.Ratings
hist(location)
#Here it is clear that our problem relies in the way we collected data
#Infact, in our scraping program we added automatic values of 8
#to properties who were lacking ratings and location ratings

#This now creates a distribution which is non-sense and that
#is very unrealistic.

#Though the value of 8 may have been realistic as a mean for
#general ratings, as we can see a location rating of 8 is very low
#compared to the other values as our data is from Rome
#which hardly is not appreciated as a location

#We thus decide to completely disregard this variables,
#as many of the values were automatically chosen to be 8.0

table(location)

#507 values (or something less) were manually given, so it does not 
#make sense to substitute each of them with the mean.

#Question: can we still use the valid data in some way?----


model <- polr(
  score ~ Discount.. +
    Ratings + Number.of.Reviews +
    Genius + Preferred.Partner +
    Distance*Nightly.Price + Stars*Nightly.Price +
    Free.Cancellation + Pay.later +
    Travel.Sustainability + Rental.type,
  data = new_df,
  Hess = TRUE
)

summary(model)

#PREDICTION----
linear_predictor <- predict(model, newdata = new_df)

table(linear_predictor)
#We are happy with the table as all categories show a similar number of predicted values

#Let's further check our results on the training data
abs_errors <- abs(as.numeric(new_df$score) - as.numeric(linear_predictor))
table(abs_errors)
sum(abs_errors <= 0)

#Almost 75% of our data has been predicted to be less than 20% off of its real position
#In practice, this mean being 200 positions off - 8 search pages.

#We compute the concordance index which gives us an idea of the comparison
#of each unit's predicted probabilities with its actual class

c_index <- concordance(Surv(as.numeric(new_df$score)) ~ as.numeric(linear_predictor))
c_index$concordance
#0.7543787

probabilities <- predict(model, newdata = new_df, type = 'probs')
View(probabilities)

means <- numeric(length = 1007)
for (i in 1:nrow(probabilities)){
  x <- 0
  for (j in 1:ncol(probabilities)){
    x = x + probabilities[i,j]*as.numeric(j)
  }
  means[i] <- x
}

cor(means, as.numeric(new_df$score))

#We get a multiple correlation coefficient of 0.645453
#which indicates positive correlation, but a far from perfect one

plot(as.numeric(linear_predictor))
#It looks like it follows the correct trend.

#TEST FOR PROPORTIONAL ODDS----
#Use LRT 
#We fit a multinomial model and compare it with the POLR.
multinom_model <- multinom(score ~ Discount.. +
                             Ratings + Number.of.Reviews +
                             Genius + Preferred.Partner +
                             Distance*Nightly.Price + Stars*Nightly.Price +
                             Free.Cancellation + Pay.later +
                             Travel.Sustainability + Rental.type,
                           data = new_df, Hess = T, model = T)

summary(multinom_model)

# Calculate log likelihood manually
M1 <- sum(log(predict(model, type = "probs")[cbind(1:length(data$y), as.numeric(data$y))]))

# View log likelihood
log_likelihood
M2 <- logLik(multinom_model)  #'log Lik.' -1928.148 (df=216)
G <- -2*(M1[1] - M2[1]) #I used a block bracket here in the real code
# 26.8099283
pchisq(G,12,lower.tail = FALSE)

M2[1]

LR.test(model, call=FALSE)
