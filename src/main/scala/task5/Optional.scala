package task5

object Optionals:
  /**
   * Optional è un tipo che rappresenta un valore che può essere presente o meno.
   * Simile a Optional in Java ma usando il concetto di ADT.
   * Quindi, un Optional è un tipo somma con due casi: Maybe e Empty.
   * Maybe contiene il valore, e Empty rappresenta l'assenza di un valore.
   *
   * @tparam A
   */
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    /**
     * isEmpty ritorna true se l'opzionale è Empty, false altrimenti.
     * Esempio:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional l'opzionale da controllare
     * @tparam A il tipo dell'opzionale
     * @return true se l'opzionale è Empty, false altrimenti
     */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _ => false

    /**
     * orElse ritorna il valore dell'opzionale se è Maybe, altrimenti ritorna il valore di default.
     * Esempio:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional l'opzionale da ottenere il valore
     * @param default il valore di default da ritornare se l'opzionale è Empty
     * @tparam A il tipo dell'opzionale
     * @tparam B il tipo del valore di default
     * @return il valore dell'opzionale se è Maybe, altrimenti il valore di default
     */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty() => default

    /**
     * map applica la funzione f al valore dell'opzionale se è Maybe, altrimenti ritorna Empty.
     * Esempio:
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     * @param optional l'opzionale su cui applicare la funzione
     * @param f la funzione da applicare al valore dell'opzionale
     * @tparam A il tipo dell'opzionale
     * @tparam B il tipo del risultato della funzione
     * @return il risultato dell'applicazione della funzione al valore dell'opzionale se è Maybe, altrimenti Empty
     */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
      case Maybe(value) => Maybe(f(value))
      case Empty() => Empty()

    /**
     * filter restituisce il valore dell'opzionale se è Maybe e soddisfa il predicato, altrimenti ritorna Empty.
     * Esempio:
     * filter(Maybe(5))(_ > 2) == Maybe(5)
     * filter(Maybe(5))(_ > 8) == Empty()
     * filter(Empty())(_ > 2) == Empty()
     *
     * @param optional l'opzionale su cui applicare il predicato
     * @param predicate il predicato da applicare al valore dell'opzionale
     * @tparam A il tipo dell'opzionale
     * @return l'opzionale con il valore se è Maybe e soddisfa il predicato, altrimenti Empty
     */
    def filter[A](optional: Optional[A], predicate: A => Boolean): Optional[A] = optional match
      case Maybe(value) if predicate(value) => Maybe(value)
      case _ => Empty()
