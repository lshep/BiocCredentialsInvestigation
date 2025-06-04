## Command Line Bash

## Get BiocCredentials user list
curl -u <admin email>:<admin password> https://git.bioconductor.org/BiocCredentials/api/biocusers/ > biocusers.json

## Copy gitolite config file of currently active packages
cp ~/BioconductorPackages/PkgManagement/gitolite-admin/conf/packages.conf .


## In R

library(jsonlite)
library(stringr)
library(dplyr)

## Load json file of BiocCredentials
json_file = "biocusers.json"
json_data = fromJSON(json_file,flatten=TRUE)

## unknown github ids
length(which(is.na(json_data$github_username)))
## [1] 669

## Load and configure packages.conf file to map currently active package maintainers
pkgs_conf = "packages.conf"
conn = file(pkgs_conf, open="r")
lines = str_trim(readLines(conn))
close(conn)
df = data.frame(pkg="", usernames="")
pkg = ""
for(l in lines){
    if(startsWith(l, "repo")){
        pkg = tail(strsplit(l, "/")[[1]],1)
    }
    if (startsWith(l, "RW devel")) {
        user_string <- sub("RW devel =\\s*", "", l)
        usernames <- strsplit(user_string, "\\s+")[[1]]
        df = rbind(df,data.frame(pkg, usernames))
    }
}
df = df[-1,]

df$usernames <- as.character(df$usernames)
json_data$username <- as.character(json_data$username)

# Merge on usernames and username to get github_username
df_merged_username <- merge(df, json_data,
                   by.x = "usernames", by.y = "username", all.x = TRUE)

df_merged_pkg <- df_merged_username %>% arrange(pkg)%>% select(pkg, everything())

## how many packages don't have a github id
## however this includes @bioconductor_writers
## which is a shorthand for all the core team 
> length(which(is.na(df_merged_pkg$github_username)))
[1] 809
> length(which(is.na(df_merged_username$github_username)))
[1] 809

# remove @bioconductor_writers
> length(setdiff(which(is.na(df_merged_username$github_username)),
                 which(df_merged_username$usernames == "@bioconductor_writers")))
[1] 699

# but this could include potentially a package that has a maintainer with a
# valid maintainer with github_id and maintainer without a valid github_id

df_unique_pkg <- df_merged_pkg %>%
  group_by(pkg) %>%
  summarise(
    github_usernames = {
      g_users <- unique(github_username)
      g_users <- g_users[!is.na(g_users)]

      if (length(g_users) > 0) {
        paste(g_users, collapse = ", ")
      } else {
        user_names <- unique(usernames)
        if (length(user_names) == 1 && user_names == "@bioconductor_writers") {
          "@bioconductor_writers"
        } else {
          NA_character_
        }
      }
    },
    .groups = "drop"
  )


#> dim(df_unique_pkg)
#[1] 3035    2

## Which packages have no github username 
> length(which(is.na(df_unique_pkg$github_usernames)))
[1] 388

no_githubid_pkg =
    as.list(df_unique_pkg[which(is.na(df_unique_pkg$github_usernames)),"pkg"])


save.image("Results.Rdata")


> print(df_unique_pkg[which(is.na(df_unique_pkg$github_usernames)), "pkg"], n = Inf)
# A tibble: 388 × 1
    pkg                         
    <chr>                       
  1 ABarray                     
  2 AGDEX                       
  3 AHCytoBands                 
  4 AIMS                        
  5 AMOUNTAIN                   
  6 ASAFE                       
  7 ASGSCA                      
  8 AgiMicroRna                 
  9 Anaquin                     
 10 AnnotationHub               
 11 AshkenazimSonChr21          
 12 BADER                       
 13 BAGS                        
 14 BCRANK                      
 15 BEAT                        
 16 BSgenomeForge               
 17 BUMHMM                      
 18 BUS                         
 19 BayesKnockdown              
 20 BiRewire                    
 21 BiSeq                       
 22 BicARE                      
 23 BioNet                      
 24 BiocIO                      
 25 BiocParallel                
 26 CAFE                        
 27 CAM3                        
 28 CCPROMISE                   
 29 CCl4                        
 30 CFAssay                     
 31 CGHcall                     
 32 CGHnormaliter               
 33 CLL                         
 34 CNAnorm                     
 35 CNORdt                      
 36 CNTools                     
 37 CNVPanelizer                
 38 CNVrd2                      
 39 CODEX                       
 40 COHCAPanno                  
 41 COSNet                      
 42 CRCL18                      
 43 CRImage                     
 44 CSAR                        
 45 CausalR                     
 46 CellMapper                  
 47 CellMapperData              
 48 ChIPComp                    
 49 ChromHeatMap                
 50 ClusterJudge                
 51 ConsensusClusterPlus        
 52 Cormotif                    
 53 CoverageView                
 54 DART                        
 55 DFP                         
 56 DLBCL                       
 57 DSS                         
 58 DTA                         
 59 DeconRNASeq                 
 60 DriverNet                   
 61 DrugVsDiseasedata           
 62 DvDdata                     
 63 EBarrays                    
 64 EBcoexpress                 
 65 EMDomics                    
 66 EmpiricalBrownsMethod       
 67 ExperimentHub               
 68 FANTOM3and4CAGE             
 69 FISHalyseR                  
 70 FRGEpistasis                
 71 FitHiC                      
 72 FlowSOM                     
 73 FlowSorted.CordBlood.450k   
 74 GAprediction                
 75 GEWIST                      
 76 GMRP                        
 77 GRENITS                     
 78 GSALightning                
 79 GSBenchMark                 
 80 GSEAlm                      
 81 GSReg                       
 82 GenVisR                     
 83 GeneBreak                   
 84 GeneGA                      
 85 GeneMeta                    
 86 GeneRegionScan              
 87 GeneSelectMMD               
 88 GlobalAncova                
 89 GraphAlignment              
 90 HD2013SGI                   
 91 HEM                         
 92 HIVcDNAvantWout03           
 93 Harshlight                  
 94 HelloRanges                 
 95 HelloRangesData             
 96 HumanAffyData               
 97 HybridMTest                 
 98 IMAS                        
 99 ITALICS                     
100 ITALICSData                 
101 IVAS                        
102 IWTomics                    
103 IdeoViz                     
104 Illumina450ProbeVariants.db 
105 KCsmart                     
106 KEGGandMetacoreDzPathwaysGEO
107 KEGGdzPathwaysGEO           
108 LPE                         
109 LedPred                     
110 LiebermanAidenHiC2009       
111 LinkHD                      
112 Linnorm                     
113 LiquidAssociation           
114 LungCancerLines             
115 LymphoSeq                   
116 MADSEQ                      
117 MAGeCKFlute                 
118 MAIT                        
119 MBASED                      
120 MBAmethyl                   
121 MBCB                        
122 MBttest                     
123 MCbiclust                   
124 MEDIPS                      
125 MEDIPSData                  
126 MEDME                       
127 MMDiff2                     
128 MMDiffBamSubset             
129 MMUPHin                     
130 MODA                        
131 MWASTools                   
132 MantelCorr                  
133 MetaboSignal                
134 MethPed                     
135 MethTargetedNGS             
136 MiChip                      
137 MiPP                        
138 MineICA                     
139 MinimumDistance             
140 Mulcom                      
141 NCIgraph                    
142 NCIgraphData                
143 NGScopyData                 
144 NTW                         
145 NanoStringDiff              
146 OPWeight                    
147 OSAT                        
148 OTUbase                     
149 OmaDB                       
150 OmicCircos                  
151 OrganismDbi                 
152 Oscope                      
153 PAA                         
154 PING                        
155 PLPE                        
156 PREDA                       
157 PREDAsampledata             
158 PROMISE                     
159 PROPER                      
160 PROPS                       
161 PROcess                     
162 Path2PPI                    
163 ProData                     
164 Pviz                        
165 QDNAseq.hg19                
166 QDNAseq.mm10                
167 RBM                         
168 RCASPAR                     
169 REMP                        
170 RGMQL                       
171 RGMQLlib                    
172 RGSEA                       
173 RIVER                       
174 RLMM                        
175 RNASeqPower                 
176 RNAinteract                 
177 RNAseq123                   
178 RRBSdata                    
179 RRHO                        
180 RTCGAToolbox                
181 RUVnormalize                
182 RUVnormalizeData            
183 RareVariantVis              
184 RiboProfiling               
185 Risa                        
186 Rmagpie                     
187 Rtreemix                    
188 S4Vectors                   
189 SBMLR                       
190 SICtools                    
191 SLqPCR                      
192 SNAGEE                      
193 SNAGEEdata                  
194 SNPediaR                    
195 SPEM                        
196 SVM2CRMdata                 
197 SemDist                     
198 SomatiCAData                
199 SpeCond                     
200 SwathXtend                  
201 TCGAcrcmRNA                 
202 TCGAcrcmiRNA                
203 TCseq                       
204 TIN                         
205 TMixClust                   
206 TPP                         
207 TTMap                       
208 TargetScore                 
209 TargetScoreData             
210 UNDO                        
211 UniProt.ws                  
212 VanillaICE                  
213 VariantTools                
214 VariantToolsData            
215 VegaMC                      
216 WES.1KG.WUGSC               
217 XDE                         
218 aCGH                        
219 acde                        
220 affyILM                     
221 agilp                       
222 annotatr                    
223 banocc                      
224 biovizBase                  
225 blima                       
226 blimaTestingData            
227 cMap2data                   
228 cancerclass                 
229 cancerdata                  
230 cbaf                        
231 ccrepe                      
232 cellity                     
233 cghMCR                      
234 chipseq                     
235 chopsticks                  
236 chromDraw                   
237 chromPlot                   
238 clippda                     
239 clst                        
240 clstutils                   
241 clustComp                   
242 cnvGSA                      
243 cnvGSAdata                  
244 coGPS                       
245 conumee                     
246 covRNA                      
247 cpvSNP                      
248 ctc                         
249 ctsGE                       
250 cummeRbund                  
251 curatedBreastData           
252 dcGSA                       
253 denyranges                  
254 diffGeneAnalysis            
255 diffloopdata                
256 dks                         
257 drawCell                    
258 dyebias                     
259 dyebiasexamples             
260 eudysbiome                  
261 fCI                         
262 fdrame                      
263 flowBeads                   
264 flowCHIC                    
265 flowCyBar                   
266 flowMeans                   
267 flowPloidy                  
268 flowPloidyData              
269 flowPlots                   
270 garfield                    
271 gcapc                       
272 gcrma                       
273 genArise                    
274 geneAttribution             
275 geneRxCluster               
276 genoCN                      
277 gep2pep                     
278 gespeR                      
279 ggbio                       
280 heatmaps                    
281 hgu133plus2CellScore        
282 hierGWAS                    
283 hopach                      
284 humanStemCell               
285 hyperdraw                   
286 iASeq                       
287 iGC                         
288 ibh                         
289 idiogram                    
290 immunoClust                 
291 impute                      
292 interactiveDisplayBase      
293 isobar                      
294 keggorthology               
295 lapmix                      
296 lmdme                       
297 lpNet                       
298 lumi                        
299 lumiBarnes                  
300 lungExpression              
301 mapscape                    
302 massiR                      
303 mdqc                        
304 metahdep                    
305 methylMnM                   
306 methylSig                   
307 mfa                         
308 miRNApath                   
309 microRNA                    
310 mirIntegrator               
311 mosaics                     
312 mosaicsExample              
313 motifbreakR                 
314 msgbsR                      
315 msmsEDA                     
316 msmsTests                   
317 multiscan                   
318 muscle                      
319 netDx                       
320 netprioR                    
321 normalize450K               
322 npGSEA                      
323 occugene                    
324 odseq                       
325 oligoClasses                
326 omicplotR                   
327 paircompviz                 
328 panp                        
329 pathifier                   
330 peco                        
331 pepDat                      
332 pepStat                     
333 pgca                        
334 phenoTest                   
335 phenopath                   
336 plier                       
337 pmm                         
338 powerTCR                    
339 prebs                       
340 prebsdata                   
341 psygenet2r                  
342 pvac                        
343 pvca                        
344 qusage                      
345 rCGH                        
346 rain                        
347 rbsurv                      
348 rfPred                      
349 rheumaticConditionWOLLBOLD  
350 rnaseqcomp                  
351 rqt                         
352 rsbml                       
353 runibic                     
354 sagenhaft                   
355 seqPattern                  
356 seqTools                    
357 seqsetvis                   
358 serumStimulation            
359 sigsquared                  
360 simpIntLists                
361 sizepower                   
362 skewr                       
363 spikeLI                     
364 sscu                        
365 ssize                       
366 staRank                     
367 stemHypoxia                 
368 stepNorm                    
369 switchde                    
370 test-package                
371 testbiocpackage1234         
372 tigre                       
373 timecourse                  
374 timescape                   
375 tissueTreg                  
376 tkWidgets                   
377 transcriptR                 
378 traseR                      
379 uSORT                       
380 updateObject                
381 vbmp                        
382 wavClusteR                  
383 widgetTools                 
384 xmapbridge                  
385 yeastExpData                
386 yeastGSData                 
387 yeastRNASeq                 
388 zFPKM                       




## example of some that are false positives that we can resolve
> df_merged_pkg[which(df_merged_pkg$pkg=="AnnotationHub"),]
             pkg usernames              email github_username
83 AnnotationHub m.carlson mcarlson@fhcrc.org            <NA>
                               
> df_merged_pkg[which(df_merged_pkg$pkg=="BiocIO"),]
       pkg usernames email github_username
174 BiocIO dvantwisk  <NA>            <NA>


## NOTE:  The below are usernames!!!  Not github_ids!!     
    
    ## bioconductor writers substitute
    ## do we keep these; in the past these trusted had access to key bioc packages
    ## @bioconductor_writers = jmacdon khansen m.smith whuber m.lawrence

    ## Core users
    ## core team that had push access to everything
    ## @core-users = mtmorgan@fhcrc.org hpages@fhcrc.org l.shepherd LiNk-NY stvjc jennifer.wokaty
  
