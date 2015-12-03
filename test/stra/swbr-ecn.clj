(ns
 autorun.trade.proj.swbr-ecn
 (:require
  [cspbox.runtime.sys.emit.inout.inject
   :refer
   [call-fn mult-send replace-by-config]]
  [clojure.core.match :refer [match]]
  [cspbox.runtime.sys.emit.state.use
   :refer
   [lookup-state fsm-run state-get state-set]]
  [cspbox.runtime.sys.emit.group
   :refer
   [run-dispatch comp-state partial-state comb-fn-state]]
  [cspbox.trading.ecn.source.hist-data :refer [data-source-hist-day]]
  [cspbox.trading.proc.post :refer [make-post-proc]]
  [cspbox.trading.proc.action :refer [pos feedback]]
  [cspbox.trading.ind.swing-breakout :refer [swing-breakout]]))

(def
 swbr
 (swing-breakout
  {:event-count 21, :expected-width 1.5, :price-extension 2.0}))

(def long-size 1)

(def short-size -1)

(def
 data-source
 (data-source-hist-day
  {:symbol "600480",
   :count 150,
   :field [:date :close],
   :conv {:close :price}}))

(def post-proc (make-post-proc {:record true}))

(def proc-order (:proc-postion post-proc))

(def g-mailbox (clojure.core/atom {}))

(clojure.core/defn
 mail
 [& [agent5735 action5736 key5737 val5738 from5739]]
 (clojure.core/let
  [f-mail__9072__auto__ (@g-mailbox agent5735)]
  (clojure.core/when
   f-mail__9072__auto__
   (if
    (clojure.core/coll? f-mail__9072__auto__)
    (clojure.core/mapv
     (clojure.core/fn [f] (f action5736 key5737 val5738 from5739))
     f-mail__9072__auto__)
    (f-mail__9072__auto__ action5736 key5737 val5738 from5739)))))

(clojure.core/defn
 data-source5627
 [in5629 out5630 state5628]
 (clojure.core/let
  [new-f__7789__auto__ data-source]
  (clojure.core.async/thread
   (clojure.core/while
    (state-get state5628 :run)
    (clojure.core/let
     [res__7790__auto__ (new-f__7789__auto__)]
     (clojure.core/when
      (clojure.core/and res__7790__auto__ out5630)
      (clojure.core.async/>!! out5630 res__7790__auto__))
     (java.lang.Thread/sleep 200))))))

(clojure.core/defn
 mail-gen-price5631
 [chans5633 state5632]
 (clojure.core/fn
  [action5634 id5635 value5636 from5637]
  (clojure.core/when
   (clojure.core/not= from5637 :gen-price)
   (clojure.core/condp
    clojure.core/=
    action5634
    :get
    (state-get state5632 id5635)
    :set
    (state-set state5632 id5635 value5636)
    :do
    nil
    nil))))

(clojure.core/defn
 gen-price5611
 [chans5638]
 (clojure.core/let
  [state__9051__auto__ (clojure.core/atom {:run true})]
  (clojure.core/mapv
   (clojure.core/partial
    call-fn
    chans5638
    state__9051__auto__
    g-mailbox)
   [[data-source5627
     nil
     :box-gen-price-price
     []
     [:price]
     :gen-price
     nil
     nil]
    [mail-gen-price5631 nil nil nil nil :gen-price :mailbox nil]])))

(clojure.core/defn
 swbr5639
 [in5640 out5643 state5642]
 (clojure.core/when
  in5640
  (clojure.core.async/go-loop
   []
   (clojure.core/let
    [pause5645 (state-get state5642 :sys-pause-chan)]
    (clojure.core/if-not
     pause5645
     (clojure.core/when-let
      [in-val5641 (clojure.core.async/<! in5640)]
      (clojure.core/let
       [out-val5644
        ((clojure.core/apply comb-fn-state swbr [nil :state false])
         in-val5641
         state5642)]
       (clojure.core/when
        (clojure.core/and out-val5644 out5643)
        (clojure.core.async/>! out5643 out-val5644))))
     (clojure.core.async/<! (clojure.core.async/timeout 1000)))
    (recur)))))

(clojure.core/defn
 mail-swbr-indicator5646
 [chans5648 state5647]
 (clojure.core/fn
  [action5649 id5650 value5651 from5652]
  (clojure.core/when
   (clojure.core/not= from5652 :swbr-indicator)
   (clojure.core/condp
    clojure.core/=
    action5649
    :get
    (state-get state5647 id5650)
    :set
    (state-set state5647 id5650 value5651)
    :do
    nil
    nil))))

(clojure.core/defn
 swbr-indicator5615
 [chans5653]
 (clojure.core/let
  [state__9051__auto__ (clojure.core/atom {:state :init})]
  (clojure.core/mapv
   (clojure.core/partial
    call-fn
    chans5653
    state__9051__auto__
    g-mailbox)
   [[swbr5639
     :box-gen-price-price
     :box-swbr-indicator-swbr
     [:price]
     [:swbr]
     :swbr-indicator
     nil
     nil]
    [mail-swbr-indicator5646
     nil
     nil
     nil
     nil
     :swbr-indicator
     :mailbox
     nil]])))

(clojure.core/defn
 init-swing-breakout5617
 [chans5686 state5685 out5687]
 (clojure.core/let
  [init-state5682
   (:stra-swing-breakout chans5686)
   fsm5684
   (clojure.core/letfn
    [(state-init-5688
      [out-5693 data5694]
      (clojure.core/let
       [{:keys [S p L PFL PFS]} data5694]
       (match
        [(and (>= p L) (< p PFL))
         (>= p PFL)
         (and (<= p S) (> p PFS))
         (<= p PFS)]
        [true _ _ _]
        (do
         (clojure.core/let
          [out-val5695
           ((clojure.core/fn
             [data5654]
             (clojure.core/let
              [value5655
               (clojure.core/conj
                [data5654]
                (feedback [:swbr-indicator :long])
                (pos long-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5655))))
            data5694)]
          (clojure.core/when
           (clojure.core/and out-val5695 out-5693)
           (clojure.core/let
            [out-val5695
             (clojure.core/merge
              out-val5695
              {:state :long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5693 out-val5695)))))
         {:fsm state-long-5689})
        [_ true _ _]
        (do
         (clojure.core/let
          [out-val5696
           ((clojure.core/fn
             [data5656]
             (clojure.core/let
              [value5657
               (clojure.core/conj
                [data5656]
                (feedback [:swbr-indicator :profit-from-long])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5657))))
            data5694)]
          (clojure.core/when
           (clojure.core/and out-val5696 out-5693)
           (clojure.core/let
            [out-val5696
             (clojure.core/merge
              out-val5696
              {:state :profit-from-long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5693 out-val5696)))))
         {:fsm state-profit-from-long-5691})
        [_ _ true _]
        (do
         (clojure.core/let
          [out-val5697
           ((clojure.core/fn
             [data5658]
             (clojure.core/let
              [value5659
               (clojure.core/conj
                [data5658]
                (feedback [:swbr-indicator :short])
                (pos short-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5659))))
            data5694)]
          (clojure.core/when
           (clojure.core/and out-val5697 out-5693)
           (clojure.core/let
            [out-val5697
             (clojure.core/merge
              out-val5697
              {:state :short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5693 out-val5697)))))
         {:fsm state-short-5690})
        [_ _ _ true]
        (do
         (clojure.core/let
          [out-val5698
           ((clojure.core/fn
             [data5660]
             (clojure.core/let
              [value5661
               (clojure.core/conj
                [data5660]
                (feedback [:swbr-indicator :profit-from-short])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5661))))
            data5694)]
          (clojure.core/when
           (clojure.core/and out-val5698 out-5693)
           (clojure.core/let
            [out-val5698
             (clojure.core/merge
              out-val5698
              {:state :profit-from-short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5693 out-val5698)))))
         {:fsm state-profit-from-short-5692})
        :else
        {:fsm state-init-5688})))
     (state-long-5689
      [out-5699 data5700]
      (clojure.core/let
       [{:keys [S p PFL PFS]} data5700]
       (match
        [(>= p PFL) (and (<= p S) (> p PFS)) (<= p PFS)]
        [true _ _]
        (do
         (clojure.core/let
          [out-val5701
           ((clojure.core/fn
             [data5662]
             (clojure.core/let
              [value5663
               (clojure.core/conj
                [data5662]
                (feedback [:swbr-indicator :profit-from-long])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5663))))
            data5700)]
          (clojure.core/when
           (clojure.core/and out-val5701 out-5699)
           (clojure.core/let
            [out-val5701
             (clojure.core/merge
              out-val5701
              {:state :profit-from-long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5699 out-val5701)))))
         {:fsm state-profit-from-long-5691})
        [_ true _]
        (do
         (clojure.core/let
          [out-val5702
           ((clojure.core/fn
             [data5664]
             (clojure.core/let
              [value5665
               (clojure.core/conj
                [data5664]
                (feedback [:swbr-indicator :short])
                (pos short-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5665))))
            data5700)]
          (clojure.core/when
           (clojure.core/and out-val5702 out-5699)
           (clojure.core/let
            [out-val5702
             (clojure.core/merge
              out-val5702
              {:state :short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5699 out-val5702)))))
         {:fsm state-short-5690})
        [_ _ true]
        (do
         (clojure.core/let
          [out-val5703
           ((clojure.core/fn
             [data5666]
             (clojure.core/let
              [value5667
               (clojure.core/conj
                [data5666]
                (feedback [:swbr-indicator :profit-from-short])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5667))))
            data5700)]
          (clojure.core/when
           (clojure.core/and out-val5703 out-5699)
           (clojure.core/let
            [out-val5703
             (clojure.core/merge
              out-val5703
              {:state :profit-from-short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5699 out-val5703)))))
         {:fsm state-profit-from-short-5692})
        :else
        {:fsm state-long-5689})))
     (state-short-5690
      [out-5704 data5705]
      (clojure.core/let
       [{:keys [p L PFL PFS]} data5705]
       (match
        [(< p PFS) (and (>= p L) (< p PFL)) (>= p PFL)]
        [true _ _]
        (do
         (clojure.core/let
          [out-val5706
           ((clojure.core/fn
             [data5668]
             (clojure.core/let
              [value5669
               (clojure.core/conj
                [data5668]
                (feedback [:swbr-indicator :profit-from-short])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5669))))
            data5705)]
          (clojure.core/when
           (clojure.core/and out-val5706 out-5704)
           (clojure.core/let
            [out-val5706
             (clojure.core/merge
              out-val5706
              {:state :profit-from-short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5704 out-val5706)))))
         {:fsm state-profit-from-short-5692})
        [_ true _]
        (do
         (clojure.core/let
          [out-val5707
           ((clojure.core/fn
             [data5670]
             (clojure.core/let
              [value5671
               (clojure.core/conj
                [data5670]
                (feedback [:swbr-indicator :long])
                (pos long-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5671))))
            data5705)]
          (clojure.core/when
           (clojure.core/and out-val5707 out-5704)
           (clojure.core/let
            [out-val5707
             (clojure.core/merge
              out-val5707
              {:state :long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5704 out-val5707)))))
         {:fsm state-long-5689})
        [_ _ true]
        (do
         (clojure.core/let
          [out-val5708
           ((clojure.core/fn
             [data5672]
             (clojure.core/let
              [value5673
               (clojure.core/conj
                [data5672]
                (feedback [:swbr-indicator :profit-from-long])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5673))))
            data5705)]
          (clojure.core/when
           (clojure.core/and out-val5708 out-5704)
           (clojure.core/let
            [out-val5708
             (clojure.core/merge
              out-val5708
              {:state :profit-from-long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5704 out-val5708)))))
         {:fsm state-profit-from-long-5691})
        :else
        {:fsm state-short-5690})))
     (state-profit-from-long-5691
      [out-5709 data5710]
      (clojure.core/let
       [{:keys [S p PFS]} data5710]
       (match
        [(and (<= p S) (> p PFS)) (<= p PFS)]
        [true _]
        (do
         (clojure.core/let
          [out-val5711
           ((clojure.core/fn
             [data5674]
             (clojure.core/let
              [value5675
               (clojure.core/conj
                [data5674]
                (feedback [:swbr-indicator :short])
                (pos short-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5675))))
            data5710)]
          (clojure.core/when
           (clojure.core/and out-val5711 out-5709)
           (clojure.core/let
            [out-val5711
             (clojure.core/merge
              out-val5711
              {:state :short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5709 out-val5711)))))
         {:fsm state-short-5690})
        [_ true]
        (do
         (clojure.core/let
          [out-val5712
           ((clojure.core/fn
             [data5676]
             (clojure.core/let
              [value5677
               (clojure.core/conj
                [data5676]
                (feedback [:swbr-indicator :profit-from-short])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5677))))
            data5710)]
          (clojure.core/when
           (clojure.core/and out-val5712 out-5709)
           (clojure.core/let
            [out-val5712
             (clojure.core/merge
              out-val5712
              {:state :profit-from-short, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5709 out-val5712)))))
         {:fsm state-profit-from-short-5692})
        :else
        {:fsm state-profit-from-long-5691})))
     (state-profit-from-short-5692
      [out-5713 data5714]
      (clojure.core/let
       [{:keys [p L PFL]} data5714]
       (match
        [(and (>= p L) (< p PFL)) (>= p PFL)]
        [true _]
        (do
         (clojure.core/let
          [out-val5715
           ((clojure.core/fn
             [data5678]
             (clojure.core/let
              [value5679
               (clojure.core/conj
                [data5678]
                (feedback [:swbr-indicator :long])
                (pos long-size))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5679))))
            data5714)]
          (clojure.core/when
           (clojure.core/and out-val5715 out-5713)
           (clojure.core/let
            [out-val5715
             (clojure.core/merge
              out-val5715
              {:state :long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5713 out-val5715)))))
         {:fsm state-long-5689})
        [_ true]
        (do
         (clojure.core/let
          [out-val5716
           ((clojure.core/fn
             [data5680]
             (clojure.core/let
              [value5681
               (clojure.core/conj
                [data5680]
                (feedback [:swbr-indicator :profit-from-long])
                (pos 0))]
              (clojure.core/apply
               clojure.core/merge
               (clojure.core/filter clojure.core/map? value5681))))
            data5714)]
          (clojure.core/when
           (clojure.core/and out-val5716 out-5713)
           (clojure.core/let
            [out-val5716
             (clojure.core/merge
              out-val5716
              {:state :profit-from-long, :id :stra-swing-breakout})]
            (clojure.core.async/go-loop
             []
             (clojure.core.async/>! out-5713 out-val5716)))))
         {:fsm state-profit-from-long-5691})
        :else
        {:fsm state-profit-from-short-5692})))]
    (clojure.core/fn
     the-fsm__9031__auto__
     ([] (the-fsm__9031__auto__ :init))
     ([initial-state__9032__auto__]
      {:fsm
       (lookup-state
        {:init state-init-5688,
         :long state-long-5689,
         :short state-short-5690,
         :profit-from-long state-profit-from-long-5691,
         :profit-from-short state-profit-from-short-5692}
        initial-state__9032__auto__)})))
   engine__9038__auto__
   (clojure.core/atom
    (if init-state5682 (fsm5684 init-state5682) (fsm5684)))]
  (clojure.core/defn
   swing-breakout5616
   [data5683]
   (clojure.core/swap! engine__9038__auto__ fsm-run out5687 data5683)
   nil)))

(clojure.core/defn
 swing-breakout5717
 [in5718 out5721 state5720]
 (clojure.core/when
  in5718
  (clojure.core.async/go-loop
   []
   (clojure.core/let
    [pause5723 (state-get state5720 :sys-pause-chan)]
    (clojure.core/if-not
     pause5723
     (clojure.core/when-let
      [in-val5719 (clojure.core.async/<! in5718)]
      (clojure.core/let
       [out-val5722
        ((partial-state swing-breakout5616) in-val5719 state5720)]
       (clojure.core/when
        (clojure.core/and out-val5722 out5721)
        (clojure.core.async/>! out5721 out-val5722))))
     (clojure.core.async/<! (clojure.core.async/timeout 1000)))
    (recur)))))

(clojure.core/defn
 swbr-strategy5621
 [chans5724]
 (clojure.core/let
  [state__9051__auto__ nil]
  (clojure.core/mapv
   (clojure.core/partial
    call-fn
    chans5724
    state__9051__auto__
    g-mailbox)
   [[init-swing-breakout5617
     nil
     :box-swbr-strategy-position
     nil
     [:position]
     :swbr-strategy
     :stra
     :stra-swing-breakout]
    [swing-breakout5717
     :box-swbr-indicator-swbr
     :box-swbr-strategy-position
     [:swbr]
     [:position]
     :swbr-strategy
     nil
     nil]])))

(clojure.core/defn
 proc-order5725
 [in5726 out5729 state5728]
 (clojure.core/when
  in5726
  (clojure.core.async/go-loop
   []
   (clojure.core/let
    [pause5731 (state-get state5728 :sys-pause-chan)]
    (clojure.core/if-not
     pause5731
     (clojure.core/when-let
      [in-val5727 (clojure.core.async/<! in5726)]
      (clojure.core/let
       [out-val5730 ((partial-state proc-order) in-val5727 state5728)]
       (clojure.core/when
        (clojure.core/and out-val5730 out5729)
        (clojure.core.async/>! out5729 out-val5730))))
     (clojure.core.async/<! (clojure.core.async/timeout 1000)))
    (recur)))))

(clojure.core/defn
 dummy-oms5625
 [chans5732]
 (clojure.core/let
  [state__9051__auto__ nil]
  (clojure.core/mapv
   (clojure.core/partial
    call-fn
    chans5732
    state__9051__auto__
    g-mailbox)
   [[proc-order5725
     :box-swbr-strategy-position
     nil
     [:position]
     []
     :dummy-oms
     nil
     nil]])))

(clojure.core/defn
 start
 [& [init5734]]
 (clojure.core/let
  [chans5733 (clojure.core/atom init5734)]
  nil
  (replace-by-config "replace.edn")
  (do
   (gen-price5611 chans5733)
   (swbr-indicator5615 chans5733)
   (swbr-strategy5621 chans5733)
   (dummy-oms5625 chans5733))))

(clojure.core/defn init [])

(clojure.core/defn stop [] (mail :gen-price :set :run false))

(clojure.core/defn -start [this] (future (start)))

(clojure.core/defn -stop [this] (stop))

(clojure.core/defn
 -main
 [& args]
 (println "hello, main")
 (future (start)))

(start)

(stop)

