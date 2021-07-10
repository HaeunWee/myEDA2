#' @import ggplot2 magrittr reshape2
#' @export
#'
#'
#'

mySummary <- function(v){
  if(is.factor(v)){
    v <- table(v)
    print(v)
    v <- v %>% reshape2::melt()
    colnames(v) <- c('value','count')
    return(ggplot2::ggplot(v , aes(x = value, y = count, fill = value)) + geom_bar(stat = 'identity'))
  }

  if(is.numeric(v)){
    print(summary(v))
    v <- table(v)

    v <- v %>% reshape2::melt()
    colnames(v) <- c('value','count')
    return(ggplot2::ggplot(v , aes(x = value)) + geom_histogram(binwidth = 1, fill = '#192a56', color = '#f5f6fa'))
  }
}

#컨트롤쉬프트 디  다큐먼트
#컨트롤쉬프트 비 패키지 빌드
#컨트롤 쉬프트
#네임스페이스 디스크립션 띄우느거?
