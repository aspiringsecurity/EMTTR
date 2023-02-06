import React from 'react'

const ProposalCardskeleton = () => {
  return (
    <div className="w-full relative rounded md:rounded-xl md:border border border-gray-200  text-base transition-colors border-b first:border-t">
      <div className="flex flex-col flex-1 gap-5 p-3 sm:p-4">
        <div className="mt-auto flex gap-3">
          <div className="bg-gray-200 w-5 sm:w-7 h-5 sm:h-7 animate-pulse rounded-full"></div>
          <div className="bg-gray-200 w-32 h-5 sm:h-7 animate-pulse rounded-full"></div>
          <div className="bg-gray-200 w-16 h-5 sm:h-7 animate-pulse rounded-full ml-auto"></div>
        </div>
        <div className="flex flex-1 flex-col gap-3">
          <div className="bg-gray-200 w-3/4 animate-pulse h-5 sm:h-6 rounded-2xl"></div>
          <div className="bg-gray-200 w-full animate-pulse mt-2.5 h-2.5 sm:h-3 rounded-2xl"></div>
          <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
          <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
          <div className="bg-gray-200 w-14 animate-pulse h-2.5 sm:h-3 mt-2 rounded-2xl"></div>
        </div>
      </div>
    </div>
  );
}

export default ProposalCardskeleton