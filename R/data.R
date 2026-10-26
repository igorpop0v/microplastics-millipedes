# Observed individuals; sizes are individual means in millimetres.
input_data <- utils::read.table(text = "
sample_id|life_form|polymer|input_size_raw|output_size_raw|output_count
bj ldpe 3-5 1|big_juloid|LDPE|3-5|1.83|3
bj ldpe 3-5 2|big_juloid|LDPE|3-5|1.86|9
bj ldpe 3-5 3|big_juloid|LDPE|3-5|1.878|12
bj ldpe 3-5 4|big_juloid|LDPE|3-5|1.901|5
bj ldpe 3-5 5|big_juloid|LDPE|3-5|1.922|7
bj ldpe 01-02 1|big_juloid|LDPE|0.1-0.2|0.181|1
bj ldpe 01-02 2|big_juloid|LDPE|0.1-0.2|0.169|5
bj ldpe 01-02 3|big_juloid|LDPE|0.1-0.2|0.178|4
bj ldpe 01-02 4|big_juloid|LDPE|0.1-0.2|0.172|1
bj ldpe 01-02 5|big_juloid|LDPE|0.1-0.2|0.175|2
bj pet 3-5 1|big_juloid|PET|3-5|0.412|8
bj pet 3-5 2|big_juloid|PET|3-5|0.221|15
bj pet 3-5 3|big_juloid|PET|3-5|0.389|18
bj pet 3-5 4|big_juloid|PET|3-5|0.276|9
bj pet 3-5 5|big_juloid|PET|3-5|0.287|6
bj pet 01-02 1|big_juloid|PET|0.1-0.2|0.192|1
bj pet 01-02 2|big_juloid|PET|0.1-0.2|0.183|6
bj pet 01-02 3|big_juloid|PET|0.1-0.2|0.19|5
bj pet 01-02 4|big_juloid|PET|0.1-0.2|0.185|2
bj pet 01-02 5|big_juloid|PET|0.1-0.2|0.186|2
bj pla 3-5 1|big_juloid|PLA|3-5|2.987|4
bj pla 3-5 2|big_juloid|PLA|3-5|1.402|12
bj pla 3-5 3|big_juloid|PLA|3-5|2.634|15
bj pla 3-5 4|big_juloid|PLA|3-5|1.671|6
bj pla 3-5 5|big_juloid|PLA|3-5|2.116|5
bj pla 01-02 1|big_juloid|PLA|0.1-0.2|0.162|1
bj pla 01-02 2|big_juloid|PLA|0.1-0.2|0.15|6
bj pla 01-02 3|big_juloid|PLA|0.1-0.2|0.159|5
bj pla 01-02 4|big_juloid|PLA|0.1-0.2|0.153|1
bj pla 01-02 5|big_juloid|PLA|0.1-0.2|0.156|1
bj pvc 3-5 1|big_juloid|PVC|3-5|0.812|3
bj pvc 3-5 2|big_juloid|PVC|3-5|0.271|10
bj pvc 3-5 3|big_juloid|PVC|3-5|0.734|13
bj pvc 3-5 4|big_juloid|PVC|3-5|0.318|6
bj pvc 3-5 5|big_juloid|PVC|3-5|0.555|5
bj pvc 01-02 1|big_juloid|PVC|0.1-0.2|0.154|1
bj pvc 01-02 2|big_juloid|PVC|0.1-0.2|0.144|5
bj pvc 01-02 3|big_juloid|PVC|0.1-0.2|0.151|6
bj pvc 01-02 4|big_juloid|PVC|0.1-0.2|0.147|2
bj pvc 01-02 5|big_juloid|PVC|0.1-0.2|0.149|1
bj pp 3-5 1|big_juloid|PP|3-5|0.574|6
bj pp 3-5 2|big_juloid|PP|3-5|0.163|14
bj pp 3-5 3|big_juloid|PP|3-5|0.521|17
bj pp 3-5 4|big_juloid|PP|3-5|0.207|8
bj pp 3-5 5|big_juloid|PP|3-5|0.44|6
bj pp 01-02 1|big_juloid|PP|0.1-0.2|0.184|6
bj pp 01-02 2|big_juloid|PP|0.1-0.2|0.172|14
bj pp 01-02 3|big_juloid|PP|0.1-0.2|0.181|17
bj pp 01-02 4|big_juloid|PP|0.1-0.2|0.178|8
bj pp 01-02 5|big_juloid|PP|0.1-0.2|0.175|6
bj c 1|big_juloid|control|0|0|0
bj c 2|big_juloid|control|0|0|0
bj c 3|big_juloid|control|0|0|0
bj c 4|big_juloid|control|0|0|0
bj c 5|big_juloid|control|0|0|0
mj ldpe 3-5 1|middle_juloid|LDPE|3-5|2.198|1
mj ldpe 3-5 2|middle_juloid|LDPE|3-5|0.842|6
mj ldpe 3-5 3|middle_juloid|LDPE|3-5|1.967|5
mj ldpe 3-5 4|middle_juloid|LDPE|3-5|1.118|2
mj ldpe 3-5 5|middle_juloid|LDPE|3-5|1.501|2
mj ldpe 01-02 1|middle_juloid|LDPE|0.1-0.2|0.191|5
mj ldpe 01-02 2|middle_juloid|LDPE|0.1-0.2|0.181|14
mj ldpe 01-02 3|middle_juloid|LDPE|0.1-0.2|0.189|17
mj ldpe 01-02 4|middle_juloid|LDPE|0.1-0.2|0.184|8
mj ldpe 01-02 5|middle_juloid|LDPE|0.1-0.2|0.185|6
mj pet 3-5 1|middle_juloid|PET|3-5|3.487|2
mj pet 3-5 2|middle_juloid|PET|3-5|2.512|7
mj pet 3-5 3|middle_juloid|PET|3-5|3.401|8
mj pet 3-5 4|middle_juloid|PET|3-5|2.681|1
mj pet 3-5 5|middle_juloid|PET|3-5|3.094|2
mj pet 01-02 1|middle_juloid|PET|0.1-0.2|0.098|1
mj pet 01-02 2|middle_juloid|PET|0.1-0.2|0.086|7
mj pet 01-02 3|middle_juloid|PET|0.1-0.2|0.095|8
mj pet 01-02 4|middle_juloid|PET|0.1-0.2|0.09|2
mj pet 01-02 5|middle_juloid|PET|0.1-0.2|0|0
mj pla 3-5 1|middle_juloid|PLA|3-5|0.358|1
mj pla 3-5 2|middle_juloid|PLA|3-5|0.094|6
mj pla 3-5 3|middle_juloid|PLA|3-5|0.321|5
mj pla 3-5 4|middle_juloid|PLA|3-5|0.128|2
mj pla 3-5 5|middle_juloid|PLA|3-5|0.259|2
mj pla 01-02 1|middle_juloid|PLA|0.1-0.2|0.199|1
mj pla 01-02 2|middle_juloid|PLA|0.1-0.2|0.198|4
mj pla 01-02 3|middle_juloid|PLA|0.1-0.2|0.197|3
mj pla 01-02 4|middle_juloid|PLA|0.1-0.2|0.199|1
mj pla 01-02 5|middle_juloid|PLA|0.1-0.2|0|0
mj pvc 3-5 1|middle_juloid|PVC|3-5|0.032|5
mj pvc 3-5 2|middle_juloid|PVC|3-5|0.187|13
mj pvc 3-5 3|middle_juloid|PVC|3-5|0.114|16
mj pvc 3-5 4|middle_juloid|PVC|3-5|0.221|7
mj pvc 3-5 5|middle_juloid|PVC|3-5|0.166|5
mj pvc 01-02 1|middle_juloid|PVC|0.1-0.2|0.179|4
mj pvc 01-02 2|middle_juloid|PVC|0.1-0.2|0.167|12
mj pvc 01-02 3|middle_juloid|PVC|0.1-0.2|0.176|15
mj pvc 01-02 4|middle_juloid|PVC|0.1-0.2|0.171|6
mj pvc 01-02 5|middle_juloid|PVC|0.1-0.2|0.172|5
mj pp 3-5 1|middle_juloid|PP|3-5|1.487|1
mj pp 3-5 2|middle_juloid|PP|3-5|0|0
mj pp 3-5 3|middle_juloid|PP|3-5|1.334|11
mj pp 3-5 4|middle_juloid|PP|3-5|0.687|3
mj pp 3-5 5|middle_juloid|PP|3-5|1.045|7
mj pp 01-02 1|middle_juloid|PP|0.1-0.2|0.129|1
mj pp 01-02 2|middle_juloid|PP|0.1-0.2|0.117|4
mj pp 01-02 3|middle_juloid|PP|0.1-0.2|0.126|3
mj pp 01-02 4|middle_juloid|PP|0.1-0.2|0.121|1
mj pp 01-02 5|middle_juloid|PP|0.1-0.2|0.123|1
mj c 1|middle_juloid|control|0.1-0.2|0|0
mj c 2|middle_juloid|control|0.1-0.2|0|0
mj c 3|middle_juloid|control|0.1-0.2|0|0
mj c 4|middle_juloid|control|0.1-0.2|0|0
mj c 5|middle_juloid|control|0.1-0.2|0|0
sj ldpe 3-5 1|small_juloid|LDPE|3-5|0.552|1
sj ldpe 3-5 2|small_juloid|LDPE|3-5|0.156|7
sj ldpe 3-5 3|small_juloid|LDPE|3-5|0.498|10
sj ldpe 3-5 4|small_juloid|LDPE|3-5|0.199|4
sj ldpe 3-5 5|small_juloid|LDPE|3-5|0|0
sj ldpe 01-02 1|small_juloid|LDPE|0.1-0.2|0.199|1
sj ldpe 01-02 2|small_juloid|LDPE|0.1-0.2|0.198|6
sj ldpe 01-02 3|small_juloid|LDPE|0.1-0.2|0.197|5
sj ldpe 01-02 4|small_juloid|LDPE|0.1-0.2|0.197|1
sj ldpe 01-02 5|small_juloid|LDPE|0.1-0.2|0.199|1
sj pet 3-5 1|small_juloid|PET|3-5|1.152|1
sj pet 3-5 2|small_juloid|PET|3-5|0.398|6
sj pet 3-5 3|small_juloid|PET|3-5|1.034|5
sj pet 3-5 4|small_juloid|PET|3-5|0.508|2
sj pet 3-5 5|small_juloid|PET|3-5|0.839|2
sj pet 01-02 1|small_juloid|PET|0.1-0.2|0.118|1
sj pet 01-02 2|small_juloid|PET|0.1-0.2|0.106|5
sj pet 01-02 3|small_juloid|PET|0.1-0.2|0.115|4
sj pet 01-02 4|small_juloid|PET|0.1-0.2|0.11|1
sj pet 01-02 5|small_juloid|PET|0.1-0.2|0.111|1
sj pla 3-5 1|small_juloid|PLA|3-5|0.641|2
sj pla 3-5 2|small_juloid|PLA|3-5|0|0
sj pla 3-5 3|small_juloid|PLA|3-5|0.579|12
sj pla 3-5 4|small_juloid|PLA|3-5|0.231|3
sj pla 3-5 5|small_juloid|PLA|3-5|0.496|8
sj pla 01-02 1|small_juloid|PLA|0.1-0.2|0.187|1
sj pla 01-02 2|small_juloid|PLA|0.1-0.2|0.175|4
sj pla 01-02 3|small_juloid|PLA|0.1-0.2|0.184|3
sj pla 01-02 4|small_juloid|PLA|0.1-0.2|0.179|2
sj pla 01-02 5|small_juloid|PLA|0.1-0.2|0.18|1
sj pvc 3-5 1|small_juloid|PVC|3-5|0.353|4
sj pvc 3-5 2|small_juloid|PVC|3-5|0.092|13
sj pvc 3-5 3|small_juloid|PVC|3-5|0.317|16
sj pvc 3-5 4|small_juloid|PVC|3-5|0.126|6
sj pvc 3-5 5|small_juloid|PVC|3-5|0.257|5
sj pvc 01-02 1|small_juloid|PVC|0.1-0.2|0.133|1
sj pvc 01-02 2|small_juloid|PVC|0.1-0.2|0.121|8
sj pvc 01-02 3|small_juloid|PVC|0.1-0.2|0.13|12
sj pvc 01-02 4|small_juloid|PVC|0.1-0.2|0.125|1
sj pvc 01-02 5|small_juloid|PVC|0.1-0.2|0|0
sj pp 3-5 1|small_juloid|PP|3-5|0.512|10
sj pp 3-5 2|small_juloid|PP|3-5|0.146|18
sj pp 3-5 3|small_juloid|PP|3-5|0.463|22
sj pp 3-5 4|small_juloid|PP|3-5|0.185|12
sj pp 3-5 5|small_juloid|PP|3-5|0.394|8
sj pp 01-02 1|small_juloid|PP|0.1-0.2|0.18|1
sj pp 01-02 2|small_juloid|PP|0.1-0.2|0.168|4
sj pp 01-02 3|small_juloid|PP|0.1-0.2|0.177|3
sj pp 01-02 4|small_juloid|PP|0.1-0.2|0.172|1
sj pp 01-02 5|small_juloid|PP|0.1-0.2|0.173|1
sj c 1|small_juloid|control|0|0|0
sj c 2|small_juloid|control|0|0|0
sj c 3|small_juloid|control|0|0|0
sj c 4|small_juloid|control|0|0|0
sj c 5|small_juloid|control|0|0|0
pd ldpe 3-5 1|polydesmoid|LDPE|3-5|2.398|1
pd ldpe 3-5 2|polydesmoid|LDPE|3-5|0|0
pd ldpe 3-5 3|polydesmoid|LDPE|3-5|2.134|10
pd ldpe 3-5 4|polydesmoid|LDPE|3-5|1.245|4
pd ldpe 3-5 5|polydesmoid|LDPE|3-5|1.666|7
pd ldpe 01-02 1|polydesmoid|LDPE|0.1-0.2|0.149|8
pd ldpe 01-02 2|polydesmoid|LDPE|0.1-0.2|0.137|15
pd ldpe 01-02 3|polydesmoid|LDPE|0.1-0.2|0.146|18
pd ldpe 01-02 4|polydesmoid|LDPE|0.1-0.2|0.141|9
pd ldpe 01-02 5|polydesmoid|LDPE|0.1-0.2|0.142|6
pd pet 3-5 1|polydesmoid|PET|3-5|0.845|4
pd pet 3-5 2|polydesmoid|PET|3-5|0.283|11
pd pet 3-5 3|polydesmoid|PET|3-5|0.763|15
pd pet 3-5 4|polydesmoid|PET|3-5|0.331|6
pd pet 3-5 5|polydesmoid|PET|3-5|0.583|4
pd pet 01-02 1|polydesmoid|PET|0.1-0.2|0.128|2
pd pet 01-02 2|polydesmoid|PET|0.1-0.2|0.11|8
pd pet 01-02 3|polydesmoid|PET|0.1-0.2|0.125|13
pd pet 01-02 4|polydesmoid|PET|0.1-0.2|0.12|1
pd pet 01-02 5|polydesmoid|PET|0.1-0.2|0.121|1
pd pla 3-5 1|polydesmoid|PLA|3-5|0.923|3
pd pla 3-5 2|polydesmoid|PLA|3-5|0.311|11
pd pla 3-5 3|polydesmoid|PLA|3-5|0.834|14
pd pla 3-5 4|polydesmoid|PLA|3-5|0.363|6
pd pla 3-5 5|polydesmoid|PLA|3-5|0.646|4
pd pla 01-02 1|polydesmoid|PLA|0.1-0.2|0.147|4
pd pla 01-02 2|polydesmoid|PLA|0.1-0.2|0.135|13
pd pla 01-02 3|polydesmoid|PLA|0.1-0.2|0.144|16
pd pla 01-02 4|polydesmoid|PLA|0.1-0.2|0.139|6
pd pla 01-02 5|polydesmoid|PLA|0.1-0.2|0.14|5
pd pvc 3-5 1|polydesmoid|PVC|3-5|0.332|1
pd pvc 3-5 2|polydesmoid|PVC|3-5|0.087|6
pd pvc 3-5 3|polydesmoid|PVC|3-5|0.298|5
pd pvc 3-5 4|polydesmoid|PVC|3-5|0.119|2
pd pvc 3-5 5|polydesmoid|PVC|3-5|0.239|2
pd pvc 01-02 1|polydesmoid|PVC|0.1-0.2|0.199|1
pd pvc 01-02 2|polydesmoid|PVC|0.1-0.2|0.187|6
pd pvc 01-02 3|polydesmoid|PVC|0.1-0.2|0.196|5
pd pvc 01-02 4|polydesmoid|PVC|0.1-0.2|0.191|1
pd pvc 01-02 5|polydesmoid|PVC|0.1-0.2|0.192|1
pd pp 3-5 1|polydesmoid|PP|3-5|0.738|5
pd pp 3-5 2|polydesmoid|PP|3-5|0.211|14
pd pp 3-5 3|polydesmoid|PP|3-5|0.667|17
pd pp 3-5 4|polydesmoid|PP|3-5|0.267|8
pd pp 3-5 5|polydesmoid|PP|3-5|0.572|6
pd pp 01-02 1|polydesmoid|PP|0.1-0.2|0.163|5
pd pp 01-02 2|polydesmoid|PP|0.1-0.2|0.151|13
pd pp 01-02 3|polydesmoid|PP|0.1-0.2|0.16|16
pd pp 01-02 4|polydesmoid|PP|0.1-0.2|0.155|7
pd pp 01-02 5|polydesmoid|PP|0.1-0.2|0.156|5
pd c 1|polydesmoid|control|0|0|0
pd c 2|polydesmoid|control|0|0|0
pd c 3|polydesmoid|control|0|0|0
pd c 4|polydesmoid|control|0|0|0
pd c 5|polydesmoid|control|0|0|0
pl ldpe 3-5 1|platydesmoid|LDPE|3-5|0|0
pl ldpe 3-5 2|platydesmoid|LDPE|3-5|0|0
pl ldpe 3-5 3|platydesmoid|LDPE|3-5|0|0
pl ldpe 3-5 4|platydesmoid|LDPE|3-5|0|0
pl ldpe 3-5 5|platydesmoid|LDPE|3-5|0|0
pl ldpe 01-02 1|platydesmoid|LDPE|0.1-0.2|0|0
pl ldpe 01-02 2|platydesmoid|LDPE|0.1-0.2|0|0
pl ldpe 01-02 3|platydesmoid|LDPE|0.1-0.2|0|0
pl ldpe 01-02 4|platydesmoid|LDPE|0.1-0.2|0|0
pl ldpe 01-02 5|platydesmoid|LDPE|0.1-0.2|0|0
pl pet 3-5 1|platydesmoid|PET|3-5|0|0
pl pet 3-5 2|platydesmoid|PET|3-5|0|0
pl pet 3-5 3|platydesmoid|PET|3-5|0|0
pl pet 3-5 4|platydesmoid|PET|3-5|0|0
pl pet 3-5 5|platydesmoid|PET|3-5|0|0
pl pet 01-02 1|platydesmoid|PET|0.1-0.2|0|0
pl pet 01-02 2|platydesmoid|PET|0.1-0.2|0|0
pl pet 01-02 3|platydesmoid|PET|0.1-0.2|0|0
pl pet 01-02 4|platydesmoid|PET|0.1-0.2|0|0
pl pet 01-02 5|platydesmoid|PET|0.1-0.2|0|0
pl pla 3-5 1|platydesmoid|PLA|3-5|0|0
pl pla 3-5 2|platydesmoid|PLA|3-5|0|0
pl pla 3-5 3|platydesmoid|PLA|3-5|0|0
pl pla 3-5 4|platydesmoid|PLA|3-5|0|0
pl pla 3-5 5|platydesmoid|PLA|3-5|0|0
pl pla 01-02 1|platydesmoid|PLA|0.1-0.2|0|0
pl pla 01-02 2|platydesmoid|PLA|0.1-0.2|0|0
pl pla 01-02 3|platydesmoid|PLA|0.1-0.2|0|0
pl pla 01-02 4|platydesmoid|PLA|0.1-0.2|0|0
pl pla 01-02 5|platydesmoid|PLA|0.1-0.2|0|0
pl pvc 3-5 1|platydesmoid|PVC|3-5|0|0
pl pvc 3-5 2|platydesmoid|PVC|3-5|0|0
pl pvc 3-5 3|platydesmoid|PVC|3-5|0|0
pl pvc 3-5 4|platydesmoid|PVC|3-5|0|0
pl pvc 3-5 5|platydesmoid|PVC|3-5|0|0
pl pvc 01-02 1|platydesmoid|PVC|0.1-0.2|0|0
pl pvc 01-02 2|platydesmoid|PVC|0.1-0.2|0|0
pl pvc 01-02 3|platydesmoid|PVC|0.1-0.2|0|0
pl pvc 01-02 4|platydesmoid|PVC|0.1-0.2|0|0
pl pvc 01-02 5|platydesmoid|PVC|0.1-0.2|0|0
pl pp 3-5 1|platydesmoid|PP|3-5|0|0
pl pp 3-5 2|platydesmoid|PP|3-5|0|0
pl pp 3-5 3|platydesmoid|PP|3-5|0|0
pl pp 3-5 4|platydesmoid|PP|3-5|0|0
pl pp 3-5 5|platydesmoid|PP|3-5|0|0
pl pp 01-02 1|platydesmoid|PP|0.1-0.2|0|0
pl pp 01-02 2|platydesmoid|PP|0.1-0.2|0|0
pl pp 01-02 3|platydesmoid|PP|0.1-0.2|0|0
pl pp 01-02 4|platydesmoid|PP|0.1-0.2|0|0
pl pp 01-02 5|platydesmoid|PP|0.1-0.2|0|0
pl c 1|platydesmoid|control|0|0|0
pl c 2|platydesmoid|control|0|0|0
pl c 3|platydesmoid|control|0|0|0
pl c 4|platydesmoid|control|0|0|0
pl c 5|platydesmoid|control|0|0|0
", header = TRUE, sep = "|", quote = "", comment.char = "", na.strings = "NA",
  colClasses = c("character", "character", "character", "character", "numeric", "integer"),
  stringsAsFactors = FALSE, check.names = FALSE)

input_data$is_control <- input_data$polymer == "control"
input_data$input_size_class <- input_data$input_size_raw
input_data$input_size_class[input_data$is_control] <- NA_character_
input_data$replicate_label <- as.integer(sub(".*[[:space:]]", "", input_data$sample_id))
input_data$output_size_mm <- input_data$output_size_raw
input_data$output_size_mm[input_data$output_count == 0L | is.na(input_data$output_size_raw) |
                            input_data$output_size_raw <= 0] <- NA_real_
input_data$input_lower_mm <- unname(c("0.1-0.2" = 0.1, "3-5" = 3)[input_data$input_size_class])
input_data$input_upper_mm <- unname(c("0.1-0.2" = 0.2, "3-5" = 5)[input_data$input_size_class])
input_data <- input_data[c("sample_id", "life_form", "polymer", "input_size_raw",
  "input_size_class", "is_control", "replicate_label", "output_size_raw", "output_count",
  "output_size_mm", "input_lower_mm", "input_upper_mm")]

# Record the interpretation of zeros and control size labels.
data_issues <- rbind(
  data.frame(
    sample_id = input_data$sample_id[input_data$output_count == 0L & input_data$output_size_raw == 0],
    issue_code = "zero_count_and_zero_size", severity = "information",
    detail = "Count and source size are both zero; size is undefined without particles.",
    action = "Keep count zero; use NA for analytical particle size."),
  data.frame(
    sample_id = input_data$sample_id[input_data$is_control & input_data$input_size_raw != "0"],
    issue_code = "control_has_nonzero_input_class", severity = "review",
    detail = "The control row contains a nonzero source input-size class.",
    action = "Classify by MP type='control'; set analytical input class and bounds to NA.")
)
