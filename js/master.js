// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.


////////
// :≡ //
////////

// The expression `x :≡ y' means that whenever one reads `x', then he could as well
// have been reading `y'. In other words, `x' is a name for `y'. For instance, after
// reading `add :≡ λx.x+1', one might understand the expression `add(1)' as
// `λx.x+1(1)', which, applying the lambda calculous, means `2' after reduction.


//////////
// Type //
//////////

// When we say `type' we mean a set of values that satisfy some invariant. To define
// a type we define algorithms to build and use instances. For instance, to define
// the type `Point', we define:
//
//   Point#mk : Int Int → Point :≡ λx,y.Point#mk(x y)
//   Point#elim : (Int Int → C) → Point → C :≡ λf,Point#mk(x y).f(x y)
//
// Then, given `p : Point :≡ Point#mk(1 2)', we may define
//
//   Point#x : Point → Int :≡ Point#elim(λx,y.x)
//
// So that `Point#x(p) = 1'.
//
// We call `Interface' the algorithms used to build and use a given type an
// `Implementation' whatever choice has been made to represent instances of this
// type.
//
// For instance, the interface of Point is Point#mk, Point#elim and Point#x so far
// but we may implement these in different ways. In JavaScript, we may choose one of:
//
//   const Point.mk = (x,y) => ({ x, y })
//   const Point.elim = (f) => (point) => f(point.x,point.y)
//
//   const Point.mk = (x,y) => ([ x, y ])
//   const Point.elim = (f) => (point) => f(point[0],point[1])
//
// or something else. What matters is to use the interface, so that flexibility is
// allowed in the implementation.


/////////
// Err //
/////////

// When an invariant is violated in the current program, an error is built in the
// hope that some other program corrects it, else the current program stops
// explaining to the developper why it stopped i.e. which invariant was violated,
// stack trace and so on.

const Err = {
    // Given any : Any, Error#mk(s) builds and throw an error.  It can conveniently be
    // used within expressions of the form: cond() || Err.mk(s).
    mk(any) { throw Error(any); },

    // Given `func() : X', `alternative : Y → Z', then `func()' throws an error, we have:
    // `Error#attempt(func alternative) :≡ alternative(err)' where `err' is the error
    // value that has been caught, else `Error#attempt(func alternative) :≡ func()',
    // while avoiding the try/catch syntax.
    attempt(func, alternative) { try { return func(); } catch(error) { return alternative(error); } }
}


/////////////////
// PlainObject //
/////////////////

// Most values are Objects, but we want to be able to distinguish a more coherent
// subset. We call these PlainObject. There are three ways to build a PlainObject:
//
// - {}
// - new Object()
// - Object.create(null)

const PlainObject = {
    // Given x, PlainObject#is(x) = true iff x : PlainObject
    is: (x) => Object.prototype.toString.call(x) === '[object Object]',

    // Given a variable x, PlainObject#check(x) raises an exception iff x is not a PlainObject.
    check: (x) => PlainObject.is(x) || Err.mk(`x is not a PlainObject. x = ${x}.`)
}


////////////
// String //
////////////

// Utility functions around String.

const Str = {
    // Given x, String#is(x) = true iff x : String
    is: (x) => typeof x === 'string',

    // Given a variable x, String#check(x) raise an exception iff x is not a string.
    check: (x) => Str.is(x) || Err.mk(`x is not a String. x = ${x}.`)
}


/////////////
// Integer //
/////////////

// Utility functions around Integer.

const Integer = {
    // Given x, Integer#is(x) = true iff x : Integer
    is: (x) => Number.isInteger(x),

    // Given a variable x, Integer#check(x) raise an exception iff x is not a integer.
    check: (x) => Integer.is(x) || Err.mk(`x is not a Integer. x = ${x}.`)
}


//////////
// Null //
//////////

// We do not use null an use undefined instead. We still have to know when a value is
// null because of external API for instance.

const Null = {
    // Given x, Null#is(x) = true iff x : Null
    is: (x) => x === null
}


///////////////
// Undefined //
///////////////

// By now, most things should be obvious.

const Undefined = {
    // Given x, Undefined#is(x) = true iff x : Undefined
    is: (x) => x === undefined,

    // Given a variable x, Undefined#check(x) raise an exception iff x is not a undefined.
    check: (x) => Undefined.is(x) || Err.mk(`x is not a Undefined. x = ${x}`)
}


///////////
// DomEl //
///////////

const DomEl = {
    // Given x, DomEl#is(x) = true iff x : HTMLElement
    is: (x) => x instanceof HTMLElement,

    // Given a variable x, DomEl#check(x) raises an exception iff x is not a DomEl.
    check: (x) => DomEl.is(x) || Err.mk(`x is not a DomEl. x = ${x}`),

    // Given a string id, DomEl.find_by_id(id) returns the first DomEl in the
    // current document that has the appropriate id, otherwise: undefined.
    find_by_id: function(id) {
        Str.check(id)
        const results = document.querySelectorAll(`#${id}`)
        if (results.length !== 1) { return undefined; }
        const result = results.item(0)
        DomEl.check(result)
        return result
    }
}


////////////
// Struct //
////////////

// We need to represent conjunctions of values that satisfy some invariant. For
// instance, we may need to represent a point `p' such that `p.x' and ̀p.y' are
// meaninful.
//
// We also need the ability to tell if some value is a point or not /i.e./ if it
// satisfies the defined invariant.
//
// We also need to define sub-types for instance to represents points such that
// `sqrt(p.x^2 + p.y^2) ≤ 1' and call them `DiskPoint'.
//
// The `Struct' construct satisfy these conditions: it allows to define types of
// conjunction of values and extend them.

class Struct {
    static mk() { return new Struct(); }
    static is(x) { return x instanceof Struct; }
    static check(x) { return Struct.is(x) || Err.mk(`x is not a Struct. x = ${x}`); }
    static elim(f) { return (struct) => { Struct.check(struct); return f(); }; }
    toString() { return 'Struct#mk()'; }
}


///////////
// Point //
///////////

// We define Point to illustrate how Struct is intended to be used. A Point is a pair
// of integers, which represent coordinates w.r.t. some reference point.

class Point extends Struct {
    constructor(x, y) {
        super()
        Integer.check(x)
        this._x = x
        Integer.check(y)
        this._y = y
    }

    // Interface
    static mk(x,y) { return new Point(x,y); }
    static is(x) { return x instanceof Point; }
    static check(x) { return Point.is(x) || Err.mk(`x is not a Point. x = ${x}`); }
    static elim(f) { return (p) => { Point.check(p); return f(p._x,p._y); }; }
    static x(point) { Point.check(point); return point._x; }
    static y(point) { Point.check(point); return point._y; }
    toString() { return `Point#mk(${this._x} ${this._y})`; }
}


/////////////
// Message //
/////////////

// A Message is a Struct that represents immutable data that may be exchanged by two
// programs. If any, it has an arbitrary content.

class Message extends Struct {
    constructor(content) {
        super()
        this._content = content
    }

    // Interface
    static mk(content) { return new Message(content); }
    static is(x) { return x instanceof Message; }
    static check(x) { return Message.is(x) || Err.mk(`x is not a Message. x = ${x}`); }
    static elim(f) { return (m) => { Message.check(m); return f(m._content); }; }
    static content(msg) { Message.check(msg); return msg._content; }
    toString() { return `Message(${this._content})`; }
}


///////////
// Actor //
///////////

// An actor may be thought of as a program with which communication only occurs by
// sending an receiving messages to it. An actor may be used to model some UI
// component that has a state and react to user inputs.

// A actor is defined by an init function that builds it initial state. We write:
// init : Init and Init :≡ Data → State where Data and State are abitrary
// types. Then, after receiving a message, it computes its reply, next state and
// designate how it will handle the next message using a transition function. We
// write: tx : Tx and Tx :≡ State Message Self → Reply × State × Tx. Finally, an
// actor is build using Actor#mk : Init Tx → Type as follows:
//
// Actor(init tx)#mk :≡
//   λdata.
//     state :≡ init data
//     self :≡
//       λmsg.
//         reply state tx :≡ tx state msg self
//         reply
//     self

class Actor {
    // Interface
    static mk(init,tx) {

        const ActorType = class extends Actor {
            constructor(data) {
                super()
                this._state = init(data)
                this._tx = tx
            }

            // Interface
            static init() { return init; }
            static tx() { return tx; }
            static mk(data) { return new ActorType(data); }
            static is(x) { return x instanceof ActorType; }
            static check(x) { return ActorType.is(x) || Err.mk(`x is not a ActorType. x = ${x}`); }
            rcv (message) {
                Message.check(message)
                const [reply, next_state, next_tx] = this._tx(this._state, message, this)
                this._state = next_state
                this._tx = next_tx
                return reply
            }
            toString() {
                // TODO: would it be possible to have the names of init and tx?
                return 'Actor#mk(init tx)'
            }
        }

        return ActorType
    }
    static is(x) { return x instanceof Actor; }
    static check(x) { return Actor.is(x) || Err.mk(`x is not a Actor. x = ${x}`); }
    static send(actor, msg) { Actor.check(actor); Message.check(msg); return actor.rcv(msg); }
    static send_async(actor, msg) { return Promise.resolve().then(() => Actor.send(actor,msg)); }
}


//////////
// SubM //
//////////

// A SubM, shorthand for SubscriptionMessage, is a Message that contains the address of an
// actor a1. If an actor a2 receives it, then whenever it emits a message m, it will
// send m to a1. We say that a1 is a subscriber of a2 and listen to its messages.

class SubM extends Message {
    constructor(actor) {
        Actor.check(actor);
        super(actor);
    }

    // Interface
    static mk(content) { return new SubM(content); }
    static is(x) { return x instanceof SubM; }
    static check(x) { return SubM.is(x) || Err.mk(`x is not a SubM. x = ${x}`); }
    static elim(f) { return (m) => { SubM.check(m); return f(m._content); }; }
    static content(m) { SubM.check(m); return m._content; }
    static send(actor1, actor2) { return Actor.send(actor1, SubM.mk(actor2)); }
    toString() { return `SubM(${this._content})`; }
}


//////////////
// ElementM //
//////////////

// An ElementM is a sub-type of Message. If actor1 sends an ElementM to actor2, then
// actor1 expects a DOM Element associated with actor2. The content of the message is
// the address of actor1.

class ElementM extends Message {
    constructor(actor) {
        Actor.check(actor);
        super(actor);
    }

    // Interface
    static mk(content) { return new ElementM(content); }
    static is(x) { return x instanceof ElementM; }
    static check(x) { return ElementM.is(x) || Err.mk(`x is not a ElementM. x = ${x}`); }
    static elim(f) { return (m) => { ElementM.check(m); return f(m._content); }; }
    static content(m) { ElementM.check(m); return m._content; }
    static send(actor1) { return Actor.send(actor1, ElementM.mk(actor1)); }
    toString() { return `ElementM(${this._content})`; }
}


////////////
// AdoptM //
////////////

// An AdoptM is a sub-type of Message. If actor receives AdoptM#mk(el) where el :
// HTMLElement, then actor is expected to add el as a child of the DOM it is
// managing. actor is now the unique owner of el.

class AdoptM extends Message {
    constructor(el) {
        DomEl.check(el);
        super(el);
    }

    // Interface
    static mk(content) { return new AdoptM(content); }
    static is(x) { return x instanceof AdoptM; }
    static check(x) { return AdoptM.is(x) || Err.mk(`x is not a AdoptM. x = ${x}`); }
    static elim(f) { return (m) => { AdoptM.check(m); return f(m._content); }; }
    static content(m) { AdoptM.check(m); return m._content; }
    static send(actor1, el) { return Actor.send(actor1, AdoptM.mk(el)); }
    toString() { return `AdoptM(${this._content})`; }
}


////////////
// ClickM //
////////////

// An ClickM is a sub-type of Message. If actor emits ClickM#mk(actor) then a
// receiving actor is expected to interpret this message as "actor has been clicked
// by the user".

class ClickM extends Message {
    constructor(actor) {
        Actor.check(actor);
        super(actor);
    }

    // Interface
    static mk(content) { return new ClickM(content); }
    static is(x) { return x instanceof ClickM; }
    static check(x) { return ClickM.is(x) || Err.mk(`x is not a ClickM. x = ${x}`); }
    static elim(f) { return (m) => { ClickM.check(m); return f(m._content); }; }
    static content(m) { ClickM.check(m); return m._content; }
    static send(actor1, actor2) { return Actor.send(actor1, ClickM.mk(actor2)); }
    toString() { return `ClickM(${this._content})`; }
}


///////////
// HideM //
///////////

// A HideM is a sub-type of Message. If an actor receives it, then it is expected to
// hide the DOM part it owns.

class HideM extends Message {
    constructor() {
        super(undefined);
    }

    // Interface
    static mk() { return new HideM(); }
    static is(x) { return x instanceof HideM; }
    static check(x) { return HideM.is(x) || Err.mk(`x is not a HideM. x = ${x}`); }
    static elim(f) { return (m) => { HideM.check(m); return f(m._content); }; }
    static content(m) { HideM.check(m); return m._content; }
    static send(actor1) { return Actor.send(actor1, HideM.mk()); }
    toString() { return `HideM(${this._content})`; }
}


//////////////
// IsEmptyM //
//////////////

// A IsEmptyM is a sub-type of Message. If an actor receives it, then it is expected
// to reply with a boolean indicating if it is empty or not.

class IsEmptyM extends Message {
    constructor() {
        super(undefined);
    }

    // Interface
    static mk() { return new IsEmptyM(); }
    static is(x) { return x instanceof IsEmptyM; }
    static check(x) { return IsEmptyM.is(x) || Err.mk(`x is not a IsEmptyM. x = ${x}`); }
    static elim(f) { return (m) => { IsEmptyM.check(m); return f(m._content); }; }
    static content(m) { IsEmptyM.check(m); return m._content; }
    static send(actor) { return Actor.send(actor, IsEmptyM.mk()); }
    toString() { return `IsEmptyM(${this._content})`; }
}


///////////
// NextM //
///////////

// A NextM is a sub-type of Message. If an actor receives it, then it is expected to
// reach a next state.

class NextM extends Message {
    constructor() {
        super(undefined);
    }

    // Interface
    static mk() { return new NextM(); }
    static is(x) { return x instanceof NextM; }
    static check(x) { return NextM.is(x) || Err.mk(`x is not a NextM. x = ${x}`); }
    static elim(f) { return (m) => { NextM.check(m); return f(m._content); }; }
    static content(m) { NextM.check(m); return m._content; }
    static send(actor) { return Actor.send(actor, NextM.mk()); }
    toString() { return `NextM(${this._content})`; }
}


///////////
// StopM //
///////////

// A StopM is a sub-type of Message. After receiving this message, an actor is
// expected to ignore all other messages and free whatever resources it claimed in
// order to be garbage collected..

class StopM extends Message {
    constructor(actor) {
        Actor.check(actor)
        super(actor);
    }

    // Interface
    static mk(actor) { return new StopM(actor); }
    static is(x) { return x instanceof StopM; }
    static check(x) { return StopM.is(x) || Err.mk(`x is not a StopM. x = ${x}`); }
    static elim(f) { return (m) => { StopM.check(m); return f(m._content); }; }
    static content(m) { StopM.check(m); return m._content; }
    static send(actor1, actor2) { return Actor.send(actor1, StopM.mk(actor2)); }
    toString() { return `StopM(${this._content})`; }
}


/////////////
// RevealM //
/////////////

// A RevealM is a sub-type of Message. After receiving this message, an actor is
// expected to make sure that the DOM it owns and was hidden to the user is now
// visible.

class RevealM extends Message {
    constructor() {
        super(undefined);
    }

    // Interface
    static mk() { return new RevealM(); }
    static is(x) { return x instanceof RevealM; }
    static check(x) { return RevealM.is(x) || Err.mk(`x is not a RevealM. x = ${x}`); }
    static elim(f) { return (m) => { RevealM.check(m); return f(m._content); }; }
    static content(m) { RevealM.check(m); return m._content; }
    static send(actor) { return Actor.send(actor, RevealM.mk()); }
    toString() { return `RevealM(${this._content})`; }
}


///////////
// ShowM //
///////////

// A ShowM is a sub-type of Message. After receiving this message, an actor is
// expected to make sure that the DOM it visible to the user, even partially.

class ShowM extends Message {
    constructor() {
        super(undefined);
    }

    // Interface
    static mk() { return new ShowM(); }
    static is(x) { return x instanceof ShowM; }
    static check(x) { return ShowM.is(x) || Err.mk(`x is not a ShowM. x = ${x}`); }
    static elim(f) { return (m) => { ShowM.check(m); return f(m._content); }; }
    static content(m) { ShowM.check(m); return m._content; }
    static send(actor) { return Actor.send(actor, ShowM.mk()); }
    toString() { return `ShowM(${this._content})`; }
}


////////////
// Logger //
////////////

// A logger is an actor which objective is to provide a convinient API to log
// messages for development purposes.

const Logger = (function(){
    const init = (_data) => undefined
    const tx = (state, msg, self) => { console.log(msg); return ['ok',state, tx]; }
    return Actor.mk(init, tx)
})()


//////////
// Sink //
//////////

// A sink is an actor which objective is to provide actors that do… nothing. They are
// useful as placeholders in order to "neutralize" actions of actors in construction.

const Sink = (function(){
    const init = (_data) => undefined
    const tx = (state, _msg, _self) => ['ok',state, tx]
    return Actor.mk(init, tx)
})()


/////////
// Toc //
/////////

// A Toc represents the table of content of the current article.

const Toc = (function(){
    const init = function(_data) {
        const result = DomEl.find_by_id('table-of-contents')
        DomEl.check(result)
        return result
    }

    const tx = function(state, msg, self) {
        const html_element = state
        if (ElementM.is(msg)) { return [html_element, state, tx]; }
        Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
    }

    return Actor.mk(init, tx)
})()


//////////////
// LastEdit //
//////////////

// A LastEdit represents the last time the current article has been edited.

const LastEdit = (function(){
    const init = function(_data) {
        const el = DomEl.find_by_id('last-edit')
        DomEl.check(el)
        return el
    }

    const tx = function(state, msg, self) {
        const html_element = state
        if (ElementM.is(msg)) { return [html_element, state, tx]; }
        Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
    }

    return Actor.mk(init, tx)
})();


////////////
// Status //
////////////

// A Status represents the status of the current article. The values and meanings are
// described in the associated HTML link.

const Status = (function(){
    const init = function(_data) {
        const el = DomEl.find_by_id('status')
        DomEl.check(el)
        return el
    }

    return Actor.mk(init, LastEdit.tx())
})();


/////////
// C21 //
/////////

// An instance of C21 represents the cell row 2 and column 1 of the grid.

const C21 = (function(){
    const init = function(_data) {
        const result = DomEl.find_by_id('c21')
        DomEl.check(result)
        return result
    }

    const tx = function(state, msg, self) {
        const html_element = state

        if (AdoptM.is(msg)) {
            const el = AdoptM.content(msg)
            html_element.appendChild(el);
            return ['ok', state, tx]
        }

        Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
    }

    return Actor.mk(init, tx)
})();


/////////
// C23 //
/////////

// An instance of C23 represents the cell row 2 and column 3 of the grid.

const C23 = (function(){
    const init = function(_data) {
        const result = DomEl.find_by_id('c23')
        DomEl.check(result)
        return result
    }

    return Actor.mk(init, C21.tx())
})();


///////////
// QaBtn //
///////////

// a QaBtn represents the button on the page which function is to capture an intent
// of the user. The intent is: "Let me review the exercises of the article".

const QaBtn = (function(){
    const init = function(_data) {
        const el = DomEl.find_by_id('qa-Btn')
        DomEl.check(el)
        const observers = new Set()
        return { el, observers }
    }

    const tx = function(state, msg, self) {
        const { el, observers } = state
        const click = ClickM.mk(self)
        const signal_observers = function() { observers.forEach((actor) => Actor.send(actor,click)); }
        el.addEventListener('click', (evt) => { evt.preventDefault(); signal_observers(); })
        const next_tx = function(state, msg, self) {
            const { el, observers } = state

            if (SubM.is(msg)) {
                const actor = SubM.content(msg)
                observers.add(actor)
                return ['ok', state, next_tx]
            }

            if (HideM.is(msg)) {
                el.classList.add('hidden')
                return ['ok', state, next_tx]
            }

            Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
        }
        return next_tx(state,msg,self)
    }

    // return Actor.mk(init, tx)
    return Sink
})();


//////////////
// Reviewer //
//////////////

// A Reviewer is an actor which objective is to offer for reviewing to the user the
// exercises found in the current article.

const Reviewer = (function(){
    const exercise_to_qa = function(node) {
        const question = node.querySelector('.question');
        const answer = node.querySelector('.answer');
        return { question, answer }
    }

    const list_all_qa = function() {
        const all_exercises = [...document.querySelectorAll('.exercise')];
        return all_exercises.map(exercise_to_qa)
    }

    const init = function(root_id) {
        const root = DomEl.find_by_id(root_id)
        DomEl.check(root)

        const question = root.querySelector('.question')
        DomEl.check(question)

        const question_content = question.querySelector('.content')
        DomEl.check(question_content)

        const reveal = question.querySelector('.reveal')
        DomEl.check(reveal)

        const stop = question.querySelector('.stop')
        DomEl.check(stop)

        const answer = root.querySelector('.answer')
        DomEl.check(answer)

        const answer_content = answer.querySelector('.content')
        DomEl.check(answer_content)

        const next = root.querySelector('.next')
        DomEl.check(next)

        const discuss = root.querySelector('.discuss')
        DomEl.check(discuss)

        const discussion = root.querySelector('.discussion')
        DomEl.check(discussion)


        const observers = new Set();

        const all_qa = list_all_qa()
        const qa_iter = all_qa.values()
        const qa_cursor = qa_iter.next()
        if (qa_cursor.done) {
            question_content.replaceChildren("No more questions.")
            answer_content.replaceChildren("No more answers.")
        }
        else {
            const qa = qa_cursor.value;
            question_content.replaceChildren(qa.question)
            answer_content.replaceChildren(qa.answer)
        }

        const hidden = 'hidden'
        root.classList.remove(hidden);
        question.classList.remove(hidden);
        answer.classList.add(hidden);

        return {
            root,
            question,
            question_content,
            reveal,
            stop,
            answer,
            answer_content,
            next,
            observers,
            all_qa,
            qa_iter,
            discuss,
            discussion
        };
    }

    const tx = function(state,msg,self) {
        const { reveal, stop, next, observers, discuss, discussion } = state

        reveal.addEventListener("click", (evt) => { evt.preventDefault(); RevealM.send(self); });
        stop.addEventListener("click", (evt) => { evt.preventDefault(); StopM.send(self,self); });
        next.addEventListener("click", (evt) => { evt.preventDefault(); NextM.send(self); });
        discuss.addEventListener("click", (evt) => { evt.preventDefault(); discussion.style.display = 'flex'; });
        discussion.addEventListener("submit", (evt) => {
            evt.preventDefault();
            const form = evt.target;
            const formData = new FormData(form);
            const data = {
                method: 'POST',
                headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
                body: new URLSearchParams(new FormData(form))
            }
            const action = form.action || window.location.href
            fetch(action, data);
            NextM.send(self);
        });

        const signal_observers = function(msg) {
            observers.forEach((actor) => Actor.send(actor,msg))
        }

        const next_tx = function(state,msg,self) {
            const { root, all_qa, question, answer, question_content, answer_content, qa_iter, observers } = state
            const hidden = 'hidden'
            const set_content = function(qa_cursor) {
                if (qa_cursor.done) {
                    question_content.replaceChildren("No more questions.")
                    answer_content.replaceChildren("No more answers.")
                }
                else {
                    const qa = qa_cursor.value;
                    question_content.replaceChildren(qa.question)
                    answer_content.replaceChildren(qa.answer)
                }
            }

            if (ShowM.is(msg)) {
                state.qa_iter = all_qa.values()
                set_content(state.qa_iter.next())
                root.classList.remove(hidden);
                return ['ok', state, next_tx]
            }

            if (HideM.is(msg)) {
                root.classList.add(hidden);
                return ['ok', state, next_tx]
            }

            if (NextM.is(msg)) {
                root.classList.remove(hidden);
                question.classList.remove(hidden);
                answer.classList.add(hidden);
                set_content(qa_iter.next())
                discussion.style.display = 'none';
                return ['ok', state, next_tx]
            }

            if (RevealM.is(msg)) {
                root.classList.remove(hidden);
                question.classList.remove(hidden);
                answer.classList.remove(hidden);
                return ['ok', state, next_tx]
            }

            if (StopM.is(msg)) {
                signal_observers(StopM.mk(self))
                return ['ok', state, next_tx]
            }

            if (IsEmptyM.is(msg)) {
                return [all_qa.length === 0, state, next_tx]
            }

            if (SubM.is(msg)) {
                observers.add(SubM.content(msg))
                return ['ok', state, next_tx]
            }

            Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
        }

        return next_tx(state,msg,self)
    }

    // return Actor.mk(init, tx)
    return Sink
})();


/////////
// Dom //
/////////

// An instance of Dom is an actor that represents the current HTML page.

const Dom = (function(){
    const init = function(_data) {
        const toc = Err.attempt(() => Toc.mk(), (_err) => undefined)
        const status = Err.attempt(() => Status.mk(), (_err) => undefined)
        const last_edit = Err.attempt(() => LastEdit.mk(), (_err) => undefined)
        const reviewer = Reviewer.mk('reviewer')
        const qa = QaBtn.mk()
        const article = DomEl.find_by_id('article')
        const c21 = C21.mk()
        const c23 = C23.mk()

        HideM.send(reviewer)
        IsEmptyM.send(reviewer) && HideM.send(qa)
        toc && AdoptM.send(c21,ElementM.send(toc))
        last_edit && AdoptM.send(c23,ElementM.send(last_edit))
        status && AdoptM.send(c23,ElementM.send(status))

        return {
            qa,
            reviewer,
            article
        }
    }

    const tx = function(state, msg, self) {
        const { qa, reviewer } = state

        SubM.send(qa, self)
        SubM.send(reviewer, self)

        const next_tx = function(state, msg, self) {
            const { qa, article, reviewer } = state

            if (ShowM.is(msg)) {
                document.body.classList.add('visible');
                return ['ok', state, next_tx]
            }

            if (ClickM.is(msg) && ClickM.content(msg) === qa) {
                article.classList.add('hidden');
                ShowM.send(reviewer)
                return ['ok', state, next_tx]
            }

            if (StopM.is(msg) && StopM.content(msg) === reviewer) {
                article.classList.remove('hidden');
                HideM.send(reviewer)
                return ['ok', state, next_tx]
            }

            Err.mk(`Unexpected msg. msg = ${JSON.stringify(msg)}`)
        }

        return next_tx(state,msg,self)
    }

    return Actor.mk(init, tx)

})();


//////////
// Main //
//////////

(function(){
    let dom = undefined;
    window.addEventListener('DOMContentLoaded', () => { dom = Dom.mk(); });
    window.addEventListener('load', () => { ShowM.send(dom); });
})();
