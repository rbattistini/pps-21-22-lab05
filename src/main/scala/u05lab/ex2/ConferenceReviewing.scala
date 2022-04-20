package u05lab.ex2

/**
 * For each article, the reviewer has to reply to all the following questions
 */
enum Question:
  case Relevance, Significance, Confidence, Final
case class Score(value: Int):
  def apply(a: Int): Int = if a < 0 then 0 else if a > 10 then 10 else a
case class ArticleReview(articleId: Int, scores: Map[Question, Score])

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
trait ConferenceReviewing:
  /**
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: ArticleReview): Unit

  /**
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(articleId: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @return the scores given to the specified article and specified question,
   *         as an (ascending-ordered) list
   */
  def orderedScores(articleId: Int, question: Question): List[Int]

  /**
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(articleId: Int): Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore,
   *         ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()
  private class ConferenceReviewingImpl extends ConferenceReviewing:
    import Question.*
    var revisions: List[ArticleReview] = List()

    override def loadReview(article: ArticleReview): Unit =
      revisions = article :: revisions

    override def loadReview(articleId: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val scores = Map(
        Relevance -> Score(relevance),
        Significance -> Score(significance),
        Confidence -> Score(confidence),
        Final -> Score(fin)
      )
      revisions = ArticleReview(articleId, scores) :: revisions

    override def orderedScores(articleId: Int, question: Question): List[Int] =
        revisions
          .collect({ case ArticleReview(`articleId`, scores) => scores(question).value})
          .sorted

    private def getFinalScores(articleId: Int) : List[Int] =
      revisions.collect({ case ArticleReview(`articleId`, scores) => scores(Question.Final).value})

    override def averageFinalScore(articleId: Int): Double =
       val l = getFinalScores(articleId); l.sum.toDouble / l.length.toDouble

    private def isAccepted(articleId: Int): Boolean =
      val averageThreshold = 5
      val relevanceThreshold = 8
      averageFinalScore(articleId) > averageThreshold &&
        revisions
          .collect({case ArticleReview(`articleId`, scores) => scores(Relevance).value})
          .exists(_ >= relevanceThreshold)

    override def acceptedArticles: Set[Int] =
      revisions
        .distinct
        .collect({case ArticleReview(id, scores) if isAccepted(id) => id})
        .toSet

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.toList.sortWith(_.intValue > _.intValue).map(f => (f, averageFinalScore(f)))

    private def averageWeightedFinalScore(articleId: Int): Double =
      val l =
         revisions
           .collect({ case ArticleReview(`articleId`, scores) =>
             scores(Question.Final).value * scores(Question.Confidence).value})
      l.sum.toDouble / (10 * l.length.toDouble)

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      revisions
        .distinct
        .map(f => (f.articleId, averageWeightedFinalScore(f.articleId)))
        .toMap
