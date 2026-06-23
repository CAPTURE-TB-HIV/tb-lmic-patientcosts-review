modelEstimates <- tidybayes::spread_draws(lmicSamps,
                                    mu[cost.id],
                                    uncndtl_mu[cost.id], 
                                    ndraws = 500, 
                                    seed = tbPtntCstSeed) %>% 
                  group_by(cost.id) %>% 
    tidybayes::median_qi(mu, uncndtl_mu)

# tidybayes::spread_draws(totalModelHurdle,
#                                           theta[cost.id],
#                                           ndraws = 500,
#                                           seed = tbPtntCstSeed) %>%
#     mutate(prob = exp(theta)) %>% 
#     group_by(cost.id) %>%
#     tidybayes::median_qi(prob) %>% select(prob) %>% range()

### compare mean cost 
# mean(modelData$cost)
# mean(modelEstimates$mu)
# mean(modelEstimates$uncndtl_mu)

library(ggplot2)
modelDataPlot <- modelData %>% mutate(cost.id = 1:nrow(modelData))

allDataPlot <- cbind(modelDataPlot, modelEstimates[,-1])
allDataPlot2 <- allDataPlot %>% mutate(overlapMu = ifelse(cost < mu.upper & 
                                                        cost > mu.lower, 
                                                        "Yes", "No"), 
                                       overlapUnMu = ifelse(cost < uncndtl_mu.upper & 
                                                              cost > uncndtl_mu.lower, 
                                                          "Yes", "No"))
ggplot() + geom_point(data = allDataPlot2, 
                      aes(x=cost, y=mu, color=overlapMu), size=2, alpha=.5) + 
    theme_minimal(base_size=18) + 
    ggtitle("Comparison of observed costs and estimated conditional costs") + 
    geom_errorbar(data=allDataPlot2, aes(x=cost, ymin=mu.lower, ymax=mu.upper, color=overlapMu), alpha=.75) + 
    scale_y_log10()  +  scale_x_log10() + 
    geom_smooth(data = allDataPlot2, 
                aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
    scale_color_manual(values=c("red", "black")) +
    labs(y="Estimated conditional costs", x="Observed costs", 
         caption = "Error bars represent 95% uncertainty intervals.", 
         color="95% UI overlaps\nobserved costs") + 
    theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
          legend.background = element_rect(fill="white", color="black"))
    

ggplot() + geom_point(data = allDataPlot2, 
                      aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
    theme_minimal(base_size=18) + 
    ggtitle("Comparison of observed costs and estimated unconditional costs") + 
    geom_errorbar(data=allDataPlot2, aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
    scale_y_log10()  +  scale_x_log10() + 
    geom_smooth(data = allDataPlot2, 
                aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
    scale_color_manual(values=c("red", "black")) +
    labs(y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
         caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
         color="95% UI overlaps\nobserved costs") + 
    theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
          legend.background = element_rect(fill="white", color="black"))

ggsave(plot=get_last_plot(), file=here("output/supplement/modelFitPlot-SA1.pdf"),
       height = 8.5,  width= 11, units="in")

###how many overlap 
round(sum(allDataPlot2$overlapUnMu =="Yes") / nrow(allDataPlot2) *100)

##############################################################################|
#### Focused plots
##############################################################################|

# pdf(file = here("output/supplement/stratModelFitPlots.pdf"))
# par(mfrow = c(3,3))


 p1 <- ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                          filter(dme.prediag == 1 & 
                                     sum(dnme.prediag:ie.contntx)==0) , 
                      aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
    theme_minimal(base_size=8) + 
    ggtitle("Comparison of observed costs and estimated unconditional costs") +
    ggtitle("Pre-diagnostic direct medical costs") +
    geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                      filter(dme.prediag == 1 & 
                                 sum(dnme.prediag:ie.contntx)==0) , aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
    scale_y_log10()  +  scale_x_log10() + 
    geom_smooth(data = allDataPlot2, 
                aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
    scale_color_manual(values=c("red", "black")) +
    labs(y="Estimated costs (log10 scale)", 
         x="", #y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
         # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
         color="95% UI overlaps\nobserved costs") + 
    theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
          legend.background = element_rect(fill="white", color="black"))

p2 <- ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                          filter(dnme.prediag == 1 & 
                                     sum(dme.prediag, ie.prediag:ie.contntx)==0) , 
                      aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
    theme_minimal(base_size=8) + 
    ggtitle("Comparison of observed costs and estimated unconditional costs") +
    ggtitle("Pre-diagnostic direct non-medical costs") +
    geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                      filter(dnme.prediag == 1 & 
                                 sum(dme.prediag, ie.prediag:ie.contntx)==0), 
                  aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
    scale_y_log10()  +  scale_x_log10() + 
    geom_smooth(data = allDataPlot2, 
                aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
    scale_color_manual(values=c("red", "black")) +
    labs(x="", y="", 
        # y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
         # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
         color="95% UI overlaps\nobserved costs") + 
    theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
          legend.background = element_rect(fill="white", color="black"))

p3<-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(ie.prediag == 1 & 
                                     sum(dme.prediag, dnme.prediag, dme.intsvtx:ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Pre-diagnostic indirect costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(ie.prediag == 1 & 
                                     sum(dme.prediag, dnme.prediag, dme.intsvtx:ie.contntx)==0), 
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(x="", y="", 
            # y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

p4 <-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(dme.intsvtx== 1 & 
                                     sum(dme.prediag:ie.prediag, dnme.intsvtx:ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Intensive treatment direct medical costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(dme.intsvtx== 1 & 
                                     sum(dme.prediag:ie.prediag, dnme.intsvtx:ie.contntx)==0), 
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(y="Estimated costs (log10 scale)",  x="",

             # y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

p5<-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(dnme.intsvtx== 1 & 
                                         sum(dme.prediag:ie.prediag, dme.intsvtx, ie.intsvtx:ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Intensive treatment direct non-medical costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(dnme.intsvtx== 1 & 
                                     sum(dme.prediag:ie.prediag, dme.intsvtx, ie.intsvtx:ie.contntx)==0), 
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(x="", y="", 
            # y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

p6<-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(ie.intsvtx== 1 & 
                                         sum(dme.prediag:ie.prediag, dme.intsvtx:dnme.intsvtx, dme.contntx:ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Intensive treatment indirect costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(ie.intsvtx== 1 & 
                                      sum(dme.prediag:ie.prediag, dme.intsvtx:dnme.intsvtx, dme.contntx:ie.contntx)==0), 
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(x="", y="", 
            # y="Estimated unconditional costs (log10 scale)", x="Observed costs (log10 scale)", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

 p7<-   ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(dme.contntx== 1 & 
                                         sum(dme.prediag:ie.intsvtx, dnme.contntx:ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Continuation treatment direct medical costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(dme.contntx== 1 & 
                                     sum(dme.prediag:ie.intsvtx, dnme.contntx:ie.contntx)==0),
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(
            y="Estimated costs (log10 scale)", 
             # y="Estimated unconditional costs (log10 scale)", 
             x="Observed costs (log10 scale)", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

p8<-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(dnme.contntx== 1 & 
                                         sum(dme.prediag:ie.intsvtx, dme.contntx, ie.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Continuation treatment direct non-medical costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(dnme.contntx== 1 & 
                                     sum(dme.prediag:ie.intsvtx, dme.contntx, ie.contntx)==0), 
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(#y="Estimated unconditional costs (log10 scale)", 
             x="Observed costs (log10 scale)",  y="", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))

p9<-    ggplot() + geom_point(data = allDataPlot2 %>% rowwise() %>% 
                              filter(ie.contntx== 1 & 
                                         sum(dme.prediag:ie.intsvtx, dme.contntx, dnme.contntx)==0), 
                          aes(x=cost, y=uncndtl_mu, color=overlapUnMu), size=2, alpha=.5) + 
        theme_minimal(base_size=8) + 
        # ggtitle("Comparison of observed costs and estimated unconditional costs") +
        ggtitle("Continuation treatment indirect costs") +
        geom_errorbar(data=allDataPlot2 %>% rowwise() %>% 
                          filter(ie.contntx== 1 & 
                          sum(dme.prediag:ie.intsvtx, dme.contntx, dnme.contntx)==0),
                      aes(x=cost, ymin=uncndtl_mu.lower, ymax=uncndtl_mu.upper, color=overlapUnMu), alpha=.75) + 
        scale_y_log10()  +  scale_x_log10() + 
        geom_smooth(data = allDataPlot2, 
                    aes(x=cost, y=cost), method = "lm", se = FALSE, col = "blue") +
        scale_color_manual(values=c("red", "black")) +
        labs(#y="Estimated unconditional costs (log10 scale)", 
            x="Observed costs (log10 scale)", y="", 
             # caption = "Error bars represent 95% uncertainty intervals.\nLine of equality depicted in blue.", 
             color="95% UI overlaps\nobserved costs") + 
        theme(legend.position = "inside", legend.position.inside = c(.9, 0.1), 
              legend.background = element_rect(fill="white", color="black"))



# dev.off()

allP <- ggpubr::ggarrange(plotlist = list(p1, p2, p3, p4, p5, p6, p7, p8, p9), common.legend = TRUE, 
                  ncol=3, nrow=3, legend = "bottom")


ggsave(allP,  file=here("output/supplement/stratModelFitPlots.pdf"),
       height = 8.5,  width= 11, units="in")





# 
# 
# print(
# ggplot() + geom_point(data = modelDataPlot[1:50,], 
#                       aes(y=cost, x=cost.id), size=2, color="salmon") + 
#     theme_minimal() + 
#     ggtitle("comparison of model input costs and model fitted costs") + 
#     geom_point(data=modelEstimates[1:50,], aes(x=cost.id, y=mu), size=1, color = "black") + 
#     geom_errorbar(data=modelEstimates[1:50,], aes(x=cost.id, ymin=mu.lower, ymax=mu.upper)) + 
#     scale_y_log10()  + coord_flip()
# )
# print(
# ggplot() + geom_point(data = modelDataPlot[51:100,], 
#                       aes(y=cost, x=cost.id), size=2, color="salmon") + 
#     theme_minimal() + 
#     ggtitle("comparison of model input costs and model fitted costs") + 
#     geom_point(data=modelEstimates[51:100,], aes(x=cost.id, y=mu), size=1, color = "black") + 
#     geom_errorbar(data=modelEstimates[51:100,], aes(x=cost.id, ymin=.lower, ymax=.upper)) + 
#     scale_y_log10()  + coord_flip()
# )
# print(
# ggplot() + geom_point(data = modelDataPlot[101:150,], 
#                       aes(y=cost, x=cost.id), size=2, color="salmon") + 
#     theme_minimal() + 
#     ggtitle("comparison of model input costs and model fitted costs") + 
#     geom_point(data=modelEstimates[101:150,], aes(x=cost.id, y=mu), size=1, color = "black") + 
#     geom_errorbar(data=modelEstimates[101:150,], aes(x=cost.id, ymin=.lower, ymax=.upper)) + 
#     scale_y_log10() + coord_flip()
# )
# 
# print(
#     ggplot() + geom_point(data = modelDataPlot[151:200,], 
#                           aes(y=cost, x=cost.id), size=2, color="salmon") + 
#         theme_minimal() + 
#         ggtitle("comparison of model input costs and model fitted costs") + 
#         geom_point(data=modelEstimates[151:200,], aes(x=cost.id, y=mu), size=1, color = "black") + 
#         geom_errorbar(data=modelEstimates[151:200,], aes(x=cost.id, ymin=.lower, ymax=.upper)) + 
#         scale_y_log10() + coord_flip()
# )
# 
# print(
#     ggplot() + geom_point(data = modelDataPlot[201:250,], 
#                           aes(y=cost, x=cost.id), size=2, color="salmon") + 
#         theme_minimal() + 
#         ggtitle("comparison of model input costs and model fitted costs") + 
#         geom_point(data=modelEstimates[201:250,], aes(x=cost.id, y=mu), size=1, color = "black") + 
#         geom_errorbar(data=modelEstimates[201:250,], aes(x=cost.id, ymin=.lower, ymax=.upper)) + 
#         scale_y_log10() + coord_flip()
# )

