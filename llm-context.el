;; -*- lexical-binding: t; -*-

(defun llm-context-result-id (tool-result)
  "Return a unique ID for `TOOL-RESULT' based on the values."
  (or (llm-chat-prompt-tool-result-call-id tool-result)
      (sxhash tool-result)))

(defun llm-context-shorten-result (tool-result max-result-size)
  "Return a cons of ID and full result, and shorten TOOL-RESULT.

MAX-RESULT-SIZE is the largest we want this result to be at the end."
  (let* ((id (llm-context-result-id tool-result))
         (result (llm-chat-prompt-tool-result-result tool-result))
         (intro (format "This result is %d chars long and has been truncated with full content stored in context ID `%s'."
                        (length result) id)))
    (setf (llm-chat-prompt-tool-result-result tool-result)
          (concat intro "\n" (substring result 0 (- max-result-size intro 3))
                  "…"))
    (cons id result)))

(defun llm-context-key (id)
  "Return the key for the prompt KV store."
  (format "llm-context %s" (car to-store)))

(defun llm-context-shorten (prompt max-result-size)
  "Make PROMPT use shorten and make available long tool results.

Leaves the last set of tool results intact.

Make sure every tool-result longer than MAX-RESULT-SIZE is turned into a
buffer."

  (cl-loop for interaction in (butlast (llm-chat-prompt-interactions prompt))
           do
           (cl-loop for tool-result in (llm-chat-prompt-interaction-tool-results interaction)
                    (if (> (length (llm-chat-prompt-tool-result-result tool-result))
                           max-result-size)
                        (let ((to-store (llm-context-shorten-result tool-result)))
                          (puthash (llm-context-key (car to-store))
                                   (cdr to-store)
                                   (llm-chat-prompt-storage-kv prompt)))
                      tool-result))))

(defun llm-context-retrieve-tool (prompt)
  "Make the LLM context retrieval tool using PROMPT for storage."
  (make-llm-tool
   :function (lambda (id) (or (gethash (llm-context-key id)
                                       (llm-chat-prompt-storage-kv prompt))
                              "Incorrect ID; nothing found."))
   :name "retrieve_tool_result"
   :description "Retrieve the full text of a truncated tool result with an ID."
   :args '((:name "id" :type string :description "Identifier taken from a truncated context."))
   :async nil))
