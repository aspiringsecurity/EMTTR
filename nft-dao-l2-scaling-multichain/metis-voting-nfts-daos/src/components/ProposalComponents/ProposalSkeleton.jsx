import React from 'react'

const ProposalSkeleton = () => {
  return (
    <div className="w-full relative text-base transition-colors mt-5">
      <div className="flex flex-col flex-1 gap-5 p-1">
        <div className="bg-gray-200 w-full animate-pulse h-3.5 sm:h-5 rounded-2xl"></div>
        <div className="bg-gray-200 w-40 animate-pulse h-3 sm:h-4 mt-2 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-14 animate-pulse h-2.5 sm:h-3 mt-2 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
        <div className="bg-gray-200 w-full animate-pulse h-2.5 sm:h-3 rounded-2xl"></div>
      </div>
    </div>
  );
}

export default ProposalSkeleton